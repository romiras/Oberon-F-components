MODULE XhtmlExporter;
(**
	project	= "BlackBox"
	organization	= "www.oberon.ch"
	contributors	= "Oberon microsystems, Romiras"
	version	= "System/Rsrc/About"
	copyright	= "System/Rsrc/About"
	license	= "Docu/BB-License"
	changes	= "
	- Added export of TextViews.View into PNG images.
	"
	issues	= "
	- bug: rd.ReadChar sometimes returns attr = NIL for a character!?
	- may em occur outside of p (as now in table fields)?
	- may br only/not occur inside of p?
	- where does the document name come from?
	- how should unknown view types be handled (extensible translator framework)?
	- how can the BlackBox converter mechanism be extended to handle complex documents, which must be converted
	   to entire subdirectories?
	- should symbolic font sizes (-1..-8) be introduced, for a direct match with HTML?
	"

**)

(*
 add the following statement to Config.Setup:
		Converters.Register("", "XhtmlExporter.ExportText", "TextViews.View", "html", {});

 supported elements & attributes: html, head, title, body, p, a(href,id), font(face,size, color), strong, em, u

 assumptions:
	- there is only one "special" non-RGB color: Ports.defaultColor
	- one specific font size (defaultSize) is used as default size
	x no embedded views are in the text, except link and target views
	x link views only contain local references within the text (i.e., StdLinks.ShowTarget('anchorname') commands)

*)

	IMPORT
		Strings, Files, Dialog, Stores, Fonts, Ports, Windows, Controllers, Services, Views, Properties,
		TextModels, TextViews, TextRulers, StdLinks, F := FreeimageViewToImage, StdLog,
		XhtmlWriters, XhtmlStdFileWriters,
		XhtmlEntitySets, XhtmlTextTableMarkers;

	CONST
		pubidLiteral = "-//W3C//DTD XHTML 1.0 Strict//EN";
		sysidLiteral = "http://www.w3.org/TR/xhtml1/DTD/strict.dtd";
		
		defaultSize = 10 * Fonts.point;	(* currently, BlackBox has no notion of default font size *)
		left = 0; center = 1; right = 2;
		
		fileExt = ".png"; (* output file format extension *)

	TYPE
		Msg* = RECORD (Properties.Message)
			string*: ARRAY 2048 OF CHAR
		END;

		Exporter = POINTER TO RECORD
			inLink: BOOLEAN;	(* this variable is used for a state machine used for translating hyperlinks *)
			inTable: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inRow: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inField: BOOLEAN;	(* this variable is used for a state machine used for translating tables *)
			inPara: BOOLEAN;	(* Generating paragraph elements is delayed so that empty elements are never created.
												This variable is used for a state machine that implements the necessary delaying.
											*)
			afterSpace: BOOLEAN;	(* this variable is used for a state machine that avoids successive spaces in an element *)
			attr: TextModels.Attributes;	(* current attributes; needed to decide whether an attribute change becomes necessary *)
			level: INTEGER;	(* indentation level *)
			currentRuler: TextRulers.Ruler;	(* most recently read ruler *)
			tabIndex: INTEGER;	(* tab index in current line *)
			wr: XhtmlWriters.Writer
		END;
	
	VAR
		documentName*: Files.Name;

	PROCEDURE Invariant (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inPara OR (e.attr = NIL), 101);	(* ~e.inPara => (e.attr = NIL) *)
		ASSERT(~e.inField OR e.inRow, 102);	(* e.inField => e.inRow *)
		ASSERT(~e.inRow OR e.inTable, 103);	(* e.inRow => e.inTable *)
		ASSERT(~e.afterSpace OR e.inPara, 104);	(* e.afterSpace => e.inPara *)
		ASSERT(e.level >= 0, 105);
		ASSERT(e.tabIndex >= -1, 106);
		ASSERT(~e.inField OR (e.tabIndex >= 0), 107);	(* e.inField => e.tabIndex >= 0 *)
		ASSERT(~e.inTable OR (e.currentRuler # NIL), 108);	(* e.inTable => (e.currentRuler # NIL) *)
		ASSERT(e.wr # NIL, 109)
	END Invariant;

	PROCEDURE ColorToString (c: INTEGER; OUT str: ARRAY OF CHAR);
		VAR red, green, blue: INTEGER; h: ARRAY 16 OF CHAR;
	BEGIN
		red := c MOD 256; c := c DIV 256;
		green := c MOD 256; c := c DIV 256;
		blue := c;
		c := red;
		c := c * 256 + green;
		c := c * 256 + blue;
		Strings.IntToStringForm(c, Strings.hexadecimal, 6, "0", FALSE, h);
		str := "#" + h
	END ColorToString;

	PROCEDURE SizeToString (s: INTEGER; OUT str: ARRAY OF CHAR);
	BEGIN	(* note: defaultSize is already handled *)
		s := s DIV Fonts.point;
		IF s < 8 THEN s := 1
		ELSIF s <= 10 THEN s := 2
		ELSIF s <= 12 THEN s := 3
		ELSIF s <= 14 THEN s := 4
		ELSIF s <= 18 THEN s := 5
		ELSIF s <= 24 THEN s := 6
		ELSE s := 7
		END;
		Strings.IntToString(s, str)
	END SizeToString;


	(*
		text = p | parsed text
		body = text | (table tr td text)
	*)

	PROCEDURE SetAttr (e: Exporter; a: TextModels.Attributes);
	(* nesting: <p> <font> <strong> <em> <u>   text   </u> </em> </strong> </font> </p> *)
	(* font has face, color, size attributes. size ranges from 1..7 *)
		CONST bold = (Fonts.normal + Fonts.bold) DIV 2;
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(e # NIL, 100);
		IF e.inPara THEN
			IF (e.attr # NIL) & (e.attr # a) THEN	(* close all special attributes *)
				IF Fonts.underline IN e.attr.font.style THEN e.wr.EndTag END;
				IF Fonts.italic IN e.attr.font.style THEN e.wr.EndTag END;
				IF e.attr.font.weight >= bold THEN e.wr.EndTag END;
				IF (e.attr.font.typeface # Fonts.default) OR (e.attr.color # Ports.defaultColor) OR (e.attr.font.size # defaultSize) THEN
					e.wr.EndTag
				END
			END;
			IF (a # NIL) & (e.attr # a) THEN	(* open new special attributes *)
				IF (a.font.typeface # Fonts.default) OR (a.color # Ports.defaultColor) OR (a.font.size # defaultSize) THEN
					e.wr.StartTag("font", XhtmlWriters.preserve);
					IF a.font.typeface # Fonts.default THEN
						IF a.font.typeface = "Arial" THEN s := a.font.typeface$ ELSE s := a.font.typeface + ", Arial" END;
						e.wr.Attr("face", s$)
					END;
					IF a.color # Ports.defaultColor THEN
						ColorToString(a.color, s); e.wr.Attr("color", s$)
					END;
					SizeToString(a.font.size, s);
					IF a.font.size # defaultSize THEN e.wr.Attr("size", s$) END
				END;
				IF a.font.weight >= bold THEN e.wr.StartTag("strong", XhtmlWriters.preserve) END;
				IF Fonts.italic IN a.font.style THEN e.wr.StartTag("em", XhtmlWriters.preserve) END;
				IF Fonts.underline IN a.font.style THEN e.wr.StartTag("u", XhtmlWriters.preserve) END
			END;
			e.attr := a
		END
	END SetAttr;

	PROCEDURE BeginPara (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inPara, 20);
		IF ~e.inLink & ~ e.inTable THEN
			e.wr.StartTag("p", XhtmlWriters.preserve)
		END;
		e.inPara := TRUE
	END BeginPara;

	PROCEDURE EndPara (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inPara, 20);
		SetAttr(e, NIL);
		IF ~e.inLink & ~e.inTable THEN
			e.wr.EndTag
		END;
		e.wr.Ln;
		e.afterSpace := FALSE;
		e.inPara := FALSE
	END EndPara;


	PROCEDURE Alignment (r: TextRulers.Ruler; tabIndex: INTEGER): INTEGER;
		VAR a: INTEGER; type: SET;
	BEGIN
		ASSERT(r # NIL, 100); ASSERT(tabIndex >= 0, 101);
		IF tabIndex < r.style.attr.tabs.len THEN
			type := r.style.attr.tabs.tab[tabIndex].type;
			IF TextRulers.centerTab IN type THEN
				a := center
			ELSIF TextRulers.rightTab IN type THEN
				a := right
			ELSE
				a := left
			END
		ELSE a := left
		END;
		RETURN a
	END Alignment;

	PROCEDURE BeginField (e: Exporter);
		VAR a: INTEGER;
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inField, 20); ASSERT(~e.inPara, 21);
		a := Alignment(e.currentRuler, e.tabIndex);
		e.wr.StartTag("td", XhtmlWriters.preserve);
		IF a = center THEN
			e.wr.Attr("align", "center")
		ELSIF a = right THEN
			e.wr.Attr("align", "right")
		END;
		e.inField := TRUE
	END BeginField;

	PROCEDURE EndField (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inField, 20); ASSERT(~e.inPara, 21);
		e.wr.EndTag;
		e.inField := FALSE
	END EndField;


	PROCEDURE BeginRow (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inRow, 20); ASSERT(~e.inField, 21); ASSERT(~e.inPara, 22);
		e.wr.StartTag("tr", XhtmlWriters.prettyPrint);
		e.inRow := TRUE
	END BeginRow;

	PROCEDURE EndRow (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inRow, 20); ASSERT(~e.inField, 21);
		e.wr.EndTag;
		e.inRow := FALSE
	END EndRow;


	PROCEDURE BeginTable (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inTable, 20); ASSERT(~e.inRow, 21); ASSERT(~e.inField, 22); ASSERT(~e.inPara, 23);
		e.wr.StartTag("table", XhtmlWriters.prettyPrint); e.wr.Attr("border", "1"); e.wr.Attr("width", "100%");
		e.inTable := TRUE
	END BeginTable;

	PROCEDURE EndTable (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inTable, 20); ASSERT(~e.inRow, 21); ASSERT(~e.inField, 22); ASSERT(~e.inPara, 23);
		e.wr.EndTag;
		e.inTable := FALSE
	END EndTable;

	PROCEDURE CompleteRow (e: Exporter);
		VAR n, i: INTEGER;
	BEGIN
		ASSERT(e # NIL, 100); ASSERT(e.inTable, 101); ASSERT(e.currentRuler # NIL, 102);
		IF ~e.inRow THEN BeginRow(e) END;
		IF e.tabIndex >= 0 THEN
			IF e.inPara THEN EndPara(e) END;
			IF ~e.inField THEN BeginField(e); e.wr.Data("&nbsp;") END;
			EndField(e)
		END;
		n := e.currentRuler.style.attr.tabs.len - e.tabIndex - 1;
		IF n >= 1 THEN	(* fill row with empty fields *)
			i := 0;
			WHILE i # n DO
				INC(e.tabIndex);
				BeginField(e); e.wr.Data("&nbsp;"); EndField(e);
				INC(i)
			END
		END;
		EndRow(e)
	END CompleteRow;

	PROCEDURE BegOfTable (e: Exporter; v: Views.View): BOOLEAN;
		VAR b: BOOLEAN;
	BEGIN
		ASSERT(e # NIL, 100);
		b := FALSE;
		IF v # NIL THEN
			b := XhtmlTextTableMarkers.IsOpenMark(v)
		END;
		RETURN b
	END BegOfTable;

	PROCEDURE EndOfTable (e: Exporter; v: Views.View): BOOLEAN;
		VAR b: BOOLEAN;
	BEGIN
		ASSERT(e # NIL, 100);
		b := FALSE;
		IF v # NIL THEN
			b := XhtmlTextTableMarkers.IsCloseMark(v)
		END;
		RETURN b
	END EndOfTable;
	
	PROCEDURE BeginHref (e: Exporter; v: StdLinks.Link);
		VAR cmd, s, ref: ARRAY 256 OF CHAR;

		PROCEDURE ExtractCmd (IN s: ARRAY OF CHAR; OUT ref: ARRAY OF CHAR);
			VAR k: INTEGER;
		BEGIN
			Strings.Find(s, "(", 0, k);
			Strings.Extract(s, 0, k, ref);
		END ExtractCmd;

		PROCEDURE ExtractLink (IN s: ARRAY OF CHAR; OUT ref: ARRAY OF CHAR);
			VAR kbeg, kend: INTEGER;
		BEGIN
			Strings.Find(s, "'", 0, kbeg); (* find first ' in string *)
			Strings.Find(s, "'", kbeg+1, kend); (* find next ' in string *)
			Strings.Extract(s, kbeg+1, kend - kbeg-1, ref);
			
			Strings.Find(ref, "/Docu/", 0, kbeg); (* find first "/Docu/" in string *)
			IF kbeg > -1 THEN Strings.Replace(ref, kbeg, 6 (*"/Docu/"*), "") END;
			
			Strings.Find(ref, "Docu/", 0, kbeg); (* find first "Docu/" in string *)
			IF kbeg > -1 THEN Strings.Replace(ref, kbeg, 5 (*"Docu/"*), "") END;
			
			Strings.Find(ref, "System", 0, kbeg); (* find first "System" in string *)
			IF kbeg > -1 THEN Strings.Replace(ref, kbeg, 6 (*"System"*), "") END
		END ExtractLink;
		
		PROCEDURE GetAnchorName (VAR ref: ARRAY OF CHAR; map: INTEGER);
		BEGIN
			CASE map OF
				1: ref := "#" + ref$
				| 2: ref := ref$ + ".html"
			ELSE
				ref := ""
			END
			(*;StdLog.String("::Anchor ->" + ref$); StdLog.Ln*)
		END GetAnchorName;
		
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		v.GetCmd(s);
		ExtractCmd (s, cmd);
		ExtractLink (s, ref);
		IF cmd = "StdLinks.ShowTarget" THEN
			GetAnchorName(ref, 1)
		ELSIF cmd # "" THEN
			GetAnchorName(ref, 2)
		ELSE
			ref := ""
		END;
		e.wr.StartTag("a", XhtmlWriters.preserve);
		IF ref # "" THEN
			e.wr.Attr("href", ref$);
		END;
		e.inLink := TRUE
	END BeginHref;

	PROCEDURE EndHref (e: Exporter);
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		e.wr.EndTag;
		e.inLink := FALSE
	END EndHref;

	PROCEDURE Id (e: Exporter; v: StdLinks.Target);
		VAR s: ARRAY 256 OF CHAR;
	BEGIN
		ASSERT(e # NIL, 100);
		ASSERT(~e.inLink, 20); ASSERT(~e.inTable, 21); ASSERT(~e.inPara, 22);
		v.GetIdent(s);
		e.wr.StartTag("a", XhtmlWriters.preserve); e.wr.Attr("id", s$)
	END Id;

	PROCEDURE GetFocusedWindow (): Windows.Window;
		VAR v: Views.View; w: Windows.Window;
	BEGIN
		w := NIL;
		v := Controllers.FocusView();
		IF v # NIL THEN
			w := Windows.dir.First();
			WHILE (w # NIL) & (w.doc.ThisView() # v) DO
				w := Windows.dir.Next(w)
			END
		END;
		RETURN w
	END GetFocusedWindow;

	PROCEDURE GetDocuName (w: Windows.Window; OUT docName: Dialog.String);
		VAR pos: INTEGER;
	BEGIN
		ASSERT(w # NIL, 20);
		docName := w.name$;
		Strings.Find(docName, ".", 0, pos);
		IF pos # - 1 THEN
			docName[pos] := 0X
		END
	END GetDocuName;

	PROCEDURE ImageName (docName: Dialog.String; viewNum: INTEGER; OUT Name: Dialog.String);
	VAR
		viewNumStr: Dialog.String;
	BEGIN
		Strings.IntToString(viewNum, viewNumStr);
		Strings.ToLower(docName, docName);
		Name := docName$ + viewNumStr$
	END ImageName;

	PROCEDURE Img (e: Exporter; Name: Dialog.String);
		CONST prefix = "img/";
	BEGIN
		ASSERT(e # NIL, 100);
		e.wr.StartTag("img", XhtmlWriters.preserve);
		e.wr.Attr("src", prefix + Name);
		e.wr.Attr("alt", '[' + Name + ']');
		e.wr.EndTag;
	END Img;

	PROCEDURE ValidImgType (TypeName: Dialog.String): BOOLEAN;
	BEGIN
		RETURN (TypeName = "OleClient.View")
			OR (TypeName = "HostBitmaps.StdView")
			OR (TypeName = "HostPictures.StdView")
			OR (TypeName = "StdLogos.View")
			OR Services.Extends(TypeName, "TextViews.View")
			OR Services.Extends(TypeName, "PlotsViews.View")
	END ValidImgType;
	
	PROCEDURE ExportAsImage (v: Views.View; IN name: ARRAY OF CHAR);
		VAR saved: BOOLEAN;
	BEGIN
		saved := F.StoreViewAsImage (v, name$, 24 (*bits*), 0);
		IF saved THEN
			(*StdLog.String(name$); StdLog.Ln*)
		ELSE
			StdLog.String(name$ + " - NOT saved!"); StdLog.Ln
		END
	END ExportAsImage;
	
	PROCEDURE View (e: Exporter; v: Views.View; VAR viewNum: INTEGER; docName: Dialog.String);
	VAR
		typeName,
		fileName: Dialog.String;
	BEGIN
		ASSERT(e # NIL, 100); ASSERT(v # NIL, 101);
		WITH v: StdLinks.Link DO
			ASSERT(v.leftSide # e.inLink, 101);	(* links must not be nested *)
			IF v.leftSide THEN	(* open link *)
				BeginHref(e, v)
			ELSE	(* close link *)
				EndHref(e)
			END
		| v: StdLinks.Target DO
			IF v.leftSide THEN Id(e, v) END	(* open anchor *)
		ELSE
			Services.GetTypeName(v, typeName);
			IF ValidImgType (typeName) THEN
			    INC(viewNum);
			    IF viewNum # 0 THEN
					ImageName (docName, viewNum, fileName); (* format base file name *)
					Img(e, fileName + fileExt); (* write img tag *)
					(* write image *)
					ExportAsImage (v, fileName + fileExt)
			    ELSE
			        StdLog.Int(viewNum);StdLog.Ln
			    END
			ELSE
				StdLog.String(typeName);StdLog.Ln
			END
		END
	END View;

	PROCEDURE ExportText* (s: Stores.Store; f: Files.File);
		VAR
			e: Exporter;
			t: TextModels.Model; rd: TextModels.Reader;
			len: INTEGER;	(* number of characters that remain to be translated *)
			ch: CHAR; str: ARRAY 256 OF CHAR;
			vNum: INTEGER;
			docName: Dialog.String; window: Windows.Window;
	BEGIN
		ASSERT(s # NIL, 20); ASSERT(f # NIL, 21); ASSERT(f.Length() = 0, 22);
		ASSERT(s IS TextViews.View, 23);
		NEW(e); e.inLink := FALSE; e.inTable := FALSE; e.inPara := FALSE; e.attr := NIL; e.afterSpace := FALSE; e.level := 0;
		
		vNum := 0;
		IF documentName # "" THEN
			docName := documentName$
		ELSE
			window := GetFocusedWindow ();
			IF window # NIL THEN
				GetDocuName (window, docName);
			ELSE
				docName := "doc0001"
			END
		END;
		
		e.wr := XhtmlStdFileWriters.New(f);
		t := s(TextViews.View).ThisModel();
		len := t.Length();
		e.wr.DocType("html", pubidLiteral, sysidLiteral);
		e.wr.Ln;
		e.wr.StartTag("html", XhtmlWriters.prettyPrint);
		e.wr.StartTag("head", XhtmlWriters.prettyPrint);
		e.wr.StartTag("title", XhtmlWriters.preserve);
		e.wr.Data(docName);
		e.wr.EndTag;	(* title *)
		e.wr.EndTag;	(* head *)
		e.wr.Ln;
		e.wr.StartTag("body", XhtmlWriters.prettyPrint);
		rd := t.NewReader(NIL);
		e.tabIndex := -1;	(* no tab read in this line yet *)
		WHILE len # 0 DO
			Invariant(e);
			rd.ReadChar(ch);
			IF (rd.view # NIL) & (rd.view IS TextRulers.Ruler) THEN e.currentRuler := rd.view(TextRulers.Ruler) END;
			IF BegOfTable(e, rd.view) THEN
				ASSERT(~e.inTable, 101);
				IF e.inPara THEN EndPara(e) END;
				BeginTable(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF EndOfTable(e, rd.view) THEN
				IF e.inRow THEN CompleteRow(e) END;
				EndTable(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF (ch = TextModels.para) OR (ch = TextModels.line) OR (ch = 0X) THEN
				IF e.inTable THEN CompleteRow(e) END;

				IF ~e.inPara THEN BeginPara(e) END;
				EndPara(e);
				e.tabIndex := -1	(* no tab read in this line yet *)
			ELSIF ch = TextModels.tab THEN
				INC(e.tabIndex);
				IF e.inTable THEN
					IF ~e.inRow THEN BeginRow(e) END;
					IF e.tabIndex >= 1 THEN	(* space until first tab is not part of a table column *)
						IF e.inField THEN
							IF e.inPara THEN EndPara(e) END
						ELSE	(* last table field is empty *)
							BeginField(e); e.wr.Data("&nbsp;")
						END;
						EndField(e)
					END
				ELSE
					IF ~e.inPara THEN BeginPara(e) END;
					e.wr.Data("&emsp;"); e.afterSpace := TRUE	(* emulate tab with three spaces *)
				END
			ELSIF ch = TextModels.viewcode THEN	(* translate rd.view into HTML *)
				IF e.inPara THEN EndPara(e) END;
				View(e, rd.view, vNum, docName)
			ELSE
				IF e.inTable THEN
					IF ~e.inRow THEN BeginRow(e) END;
					IF ~e.inField THEN BeginField(e) END
				END;
				IF ~e.inPara THEN BeginPara(e) END;
				SetAttr(e, rd.attr);
				IF (ch = " ") OR (ch = TextModels.nbspace) OR (ch = TextModels.digitspace) THEN
					IF e.afterSpace THEN e.wr.Data("&nbsp;") ELSE e.wr.Data(" "); e.afterSpace := TRUE END
				ELSIF ch = TextModels.zwspace THEN
					e.wr.Data("&zwnj;"); e.afterSpace := TRUE
				ELSIF (ch = TextModels.hyphen) OR (ch = TextModels.nbhyphen) THEN
					e.wr.Data("-"); e.afterSpace := FALSE
				ELSE
					XhtmlEntitySets.MapCharToEntity(ch, str);	(* this is the normal case of writing a character *)
					e.wr.Data(str$); e.afterSpace := FALSE
				END
			END;
			DEC(len)
		END;
		ASSERT(~e.inLink, 101); ASSERT(~e.inTable, 102);
		IF e.inPara THEN EndPara(e) END;
		e.wr.EndTag;	(* body *)
		e.wr.EndTag;	(* html *)
		e.wr.Ln
	END ExportText;

END XhtmlExporter.
