(*
	Converter of BlackBox Oberon/F documents to DokuWiki markup syntax
	Based on HostTextConv.ConvertToRichText (BBCB 1.6 RC6)
	
	Nov 2012, Romiras
*)

MODULE ConvDokuWikiExporter;

	IMPORT
		Strings, Stores, Files, Fonts, Ports, Views, Properties, Converters,
		TextModels, TextRulers, TextViews, TextMappers;
	
	CONST
		CR = 0DX; LF = 0AX; FF = 0EX; TAB = 09X;
		halfpoint = Ports.point DIV 2;
		twips = Ports.point DIV 20;
		preserveFormatting = 1; (* preserve formatting (only if not need to export a program code) *)
		preserveTabs = 2; (* don't replace by spaces for identation *)
	
	VAR
		lastHeadingSize: INTEGER;
		paragraph: BOOLEAN;
		exportOpts: SET;
	
	PROCEDURE WriteChar (w: Stores.Writer; ch: CHAR);
		VAR wch: SHORTCHAR;
	BEGIN
		IF (ch # TextModels.viewcode) & (ch # TextModels.para) THEN
			wch := SHORT(ch);
			w.WriteSChar(wch);
			(*IF wch = 0DX THEN w.WriteSChar(0AX) END*)
			(*IF ch = CR THEN w.WriteSChar(LF) END*)
		END;
	END WriteChar;
	
	PROCEDURE TextView(s: Stores.Store): Stores.Store;
	BEGIN
		IF s IS Views.View THEN RETURN Properties.ThisType(s(Views.View), "TextViews.View")
		ELSE RETURN NIL
		END
	END TextView;
	
	PROCEDURE ThisWndChar (ch: CHAR): CHAR;
	BEGIN
		IF ch >= 100X THEN
			IF (ch >= 0EF00X) & (ch <= 0EFFFX) THEN ch := CHR(ORD(ch) - 0EF00H)
			ELSIF ch =  20ACX THEN ch := 80X	(* euro *)
			ELSIF ch =  201AX THEN ch := 82X
			ELSIF ch =  0192X THEN ch := 83X
			ELSIF ch =  201EX THEN ch := 84X
			ELSIF ch =  2026X THEN ch := 85X
			ELSIF ch =  2020X THEN ch := 86X
			ELSIF ch =  2021X THEN ch := 87X
			ELSIF ch =  02C6X THEN ch := 88X
			ELSIF ch =  2030X THEN ch := 89X
			ELSIF ch =  0160X THEN ch := 8AX
			ELSIF ch =  2039X THEN ch := 8BX
			ELSIF ch =  0152X THEN ch := 8CX
			ELSIF ch =  2018X THEN ch := 91X
			ELSIF ch =  2019X THEN ch := 92X
			ELSIF ch =  201CX THEN ch := 93X
			ELSIF ch =  201DX THEN ch := 94X
			ELSIF ch =  2022X THEN ch := 95X
			ELSIF ch =  2013X THEN ch := 96X
			ELSIF ch =  2014X THEN ch := 97X
			ELSIF ch =  02DCX THEN ch := 98X
			ELSIF ch =  2122X THEN ch := 99X
			ELSIF ch =  0161X THEN ch := 9AX
			ELSIF ch =  203AX THEN ch := 9BX
			ELSIF ch =  0153X THEN ch := 9CX
			ELSIF ch =  0178X THEN ch := 9FX
			ELSE  ch := "?"
			END
		ELSIF ch = 08FX THEN ch := " "	(* digit space *)
		END;
		RETURN ch
	END ThisWndChar;
	
	PROCEDURE ConvertToRichText (in: TextViews.View; (*beg, end: INTEGER;*) exportOpts: SET; VAR out: TextModels.Model);
		VAR r: TextModels.Reader; w: TextMappers.Formatter; ch: CHAR; f: Fonts.Font;
			attr, attr0: TextModels.Attributes; col: Ports.Color; tf, atf: Fonts.Typeface; p, size, asize, offs: INTEGER;
			style, astyle: SET; weight, aweight: INTEGER; rattr, rattr0: TextRulers.Attributes; ruler: TextRulers.Ruler;
			text: TextModels.Model; firstLine, firstLine0: BOOLEAN; fonts: ARRAY 256 OF Fonts.Typeface;
			colors: ARRAY 256 OF Ports.Color; fnum, cnum, i: INTEGER;

		(*
		PROCEDURE Open (IN wr: TextMappers.Formatter; attr: TextModels.Attributes);
		BEGIN
			IF attr # NIL THEN	END
		END Open;

		PROCEDURE Close (IN wr: TextMappers.Formatter; attr: TextModels.Attributes);
		BEGIN
			IF attr # NIL THEN	END
		END Close;
		*)
		
		PROCEDURE PutControl (IN command: ARRAY OF CHAR);
		BEGIN
			(*w.WriteLn; w.WriteSString(SHORT(command));*)
			IF (command = "par") & (lastHeadingSize = 0) THEN
				IF (preserveFormatting IN exportOpts) THEN
					IF ~paragraph THEN w.WriteSString("\\ ") END;
					IF ~firstLine THEN paragraph := TRUE END;
				END;
				w.WriteLn
			ELSIF command = "b" (* open bold style *) THEN
				IF (preserveFormatting IN exportOpts) & (lastHeadingSize = 0) THEN w.WriteSString("**") END
			ELSIF command = "b0" (* close bold style *) THEN
				IF (preserveFormatting IN exportOpts) & ~firstLine & ~paragraph THEN w.WriteSString("**") END
			ELSIF command = "i" (* open italic style *) THEN
				IF (preserveFormatting IN exportOpts) THEN w.WriteSString("//") END
			ELSIF command = "i0" (* close italic style *) THEN
				IF (preserveFormatting IN exportOpts) THEN w.WriteSString("//") END
			ELSIF command = "u" (* open underline style *) THEN
				IF (preserveFormatting IN exportOpts) THEN w.WriteSString("__") END
			ELSIF command = "u0" (* close underline style *) THEN
				IF (preserveFormatting IN exportOpts) THEN w.WriteSString("__") END
			END
		END PutControl;
		
		PROCEDURE PutControlEx (IN command: ARRAY OF CHAR; n: INTEGER);
			VAR s: ARRAY 20 OF CHAR;
			
			PROCEDURE PutHeading(size: INTEGER; open: BOOLEAN);
			BEGIN
				IF open THEN w.WriteLn ELSE w.WriteSString(" ") END;
				CASE size OF
				| 1: w.WriteSString("======") (* H1 *)
				| 2: w.WriteSString("=====") (* H2 *)
				| 3: w.WriteSString("====") (* H3 *)
				| 4: w.WriteSString("===") (* H4 *)
				| 5: w.WriteSString("==") (* H5 *)
				ELSE
				END;
				IF ~open THEN paragraph := TRUE; w.WriteLn END;
			END PutHeading;
			
		BEGIN
			IF command = "fs" (* set font size *) THEN
				n := n DIV Fonts.point;
				(* when font size is big ==> using heading *)
				IF n >= 16 THEN (* H1 *)
					lastHeadingSize := 1; PutHeading(lastHeadingSize, TRUE)
				ELSIF n >= 14 THEN (* H2 *)
					lastHeadingSize := 2; PutHeading(lastHeadingSize, TRUE)
				ELSIF n >= 12 THEN (* H3 *)
					lastHeadingSize := 3; PutHeading(lastHeadingSize, TRUE)
				ELSE
					PutHeading(lastHeadingSize, FALSE);
					lastHeadingSize := 0
				END
			END;
			(*Strings.IntToString(n, s); PutControl(command + s)*)
		END PutControlEx;
		
		PROCEDURE PutChar (ch: CHAR);
		BEGIN
			IF ch >= 100X THEN
				w.WriteChar(ch)
			ELSIF ch = CR THEN
				PutControl("par"); firstLine := FALSE
			ELSE
				paragraph := FALSE;
				CASE ch OF 
				| TAB: IF (preserveTabs IN exportOpts) THEN w.WriteTab ELSE w.WriteSString("   ") END
				(*| CR: PutControl("par"); firstLine := FALSE*)
				| " ".."[", "]".."z", "|", "~": w.WriteChar(ch)
				| "\": w.WriteChar("\")
				| "{": w.WriteChar("{")
				| "}": w.WriteChar("}")
				| 8FX: (* digit space *) w.WriteChar(" ")
				| 90X: (* hyphen *) w.WriteChar("-")
				| 91X: (* non-breaking hyphen *) w.WriteChar("_")
				| 0A0X: (* non-breaking space *) w.WriteChar(" ")
				| 0ADX: (* soft hyphen *) w.WriteChar("-")
				(*
				| 0A1X..0ACX, 0AEX..0FFX:
					w.WriteSString("\'"); w.WriteIntForm(ORD(ch), TextMappers.hexadecimal, 2, "0", FALSE)
				*)
				ELSE
					IF ch = 0A7X (* SECTION SIGN *) THEN
						w.WriteChar("*")
					ELSIF (ch # TextModels.para) & (ch # TextModels.viewcode) THEN
						w.WriteChar(ch)
					END;
				END
			END;
		END PutChar;
		
		PROCEDURE PutTextRulersStyling (rattr: TextRulers.Attributes);
		BEGIN
			PutControl("pard"); (* set paragraph defaults *)
			IF rattr.left # 0 THEN
				PutControlEx("li", rattr.left DIV twips)
			END;
			IF firstLine & (rattr.first # rattr.left) THEN
				PutControlEx("fi", (rattr.first - rattr.left) DIV twips)
			END;
			IF firstLine & (rattr.lead # 0) THEN
				PutControlEx("sb", rattr.lead DIV twips)
			END;
			IF rattr.grid > Ports.point THEN
				PutControlEx("sl", rattr.grid DIV twips); PutControl("slmult1")
			END;
			IF {TextRulers.leftAdjust, TextRulers.rightAdjust} - rattr.opts = {} THEN PutControl("qj")
			ELSIF TextRulers.rightAdjust IN rattr.opts THEN PutControl("qr")
			ELSIF ~(TextRulers.leftAdjust IN rattr.opts) THEN PutControl("qc")
			END;
			IF firstLine & (TextRulers.pageBreak IN rattr.opts) THEN
				PutControl("pagebb")
			END;
			i := 0;
			WHILE i < rattr.tabs.len DO
				IF TextRulers.centerTab IN rattr.tabs.tab[i].type THEN PutControl("tqc") END; 
				IF TextRulers.rightTab IN rattr.tabs.tab[i].type THEN PutControl("tqr") END; 
				IF TextRulers.barTab IN rattr.tabs.tab[i].type THEN PutControl("tb") END; 
				PutControlEx("tx", rattr.tabs.tab[i].stop DIV twips);
				INC(i)
			END;
			(*w.WriteChar(" ")*) w.WriteLn
		END PutTextRulersStyling;
		
		PROCEDURE PutTextStylingControl (attr: TextModels.Attributes);
		BEGIN
			p := w.Pos();
			(*
			IF attr.color # col THEN
				i := 0; WHILE (i < cnum) & (colors[i] # attr.color) DO INC(i) END;
				IF i = cnum THEN colors[i] := attr.color; INC(cnum) END;
				PutControlEx("cf", i);
				col := attr.color
			END;
			*)
			atf := attr.font.typeface$; asize := attr.font.size; astyle := attr.font.style; aweight := attr.font.weight;
			(*
			IF atf # tf THEN
				i := 0; WHILE (i < fnum) & (fonts[i] # atf) DO INC(i) END;
				IF i = fnum THEN fonts[i] := atf; INC(fnum) END;
				PutControlEx("f", i);
				tf := atf
			END;
			*)
			IF asize # size THEN
				PutControlEx("fs", asize);
				size := asize
			END;
			IF astyle # style THEN
				IF (Fonts.italic IN astyle) & ~(Fonts.italic IN style) THEN PutControl("i")
				ELSIF ~(Fonts.italic IN astyle) & (Fonts.italic IN style) THEN PutControl("i0")
				END;
				IF (Fonts.underline IN astyle) & ~(Fonts.underline IN style) THEN PutControl("ul")
				ELSIF ~(Fonts.underline IN astyle) & (Fonts.underline IN style) THEN PutControl("ul0")
				END;
				IF (Fonts.strikeout IN astyle) & ~(Fonts.strikeout IN style) THEN PutControl("strike")
				ELSIF ~(Fonts.strikeout IN astyle) & (Fonts.strikeout IN style) THEN PutControl("strike0")
				END;
				style := astyle
			END;
			IF aweight # weight THEN
				IF (aweight > Fonts.normal) & (weight = Fonts.normal) THEN PutControl("b")
				ELSIF (aweight = Fonts.normal) & (weight > Fonts.normal) THEN PutControl("b0")
				END;
				weight := aweight
			END;
			(*
			IF attr.offset # offs THEN
				IF attr.offset > 0 THEN PutControlEx("up", attr.offset DIV halfpoint)
				ELSIF attr.offset < 0 THEN PutControlEx("dn", -(attr.offset DIV halfpoint))
				ELSIF offs > 0 THEN PutControl("up0")
				ELSE PutControl("dn0")
				END;
				offs := attr.offset
			END;
			*)
			IF w.Pos() # p THEN w.WriteChar(" ") END;
		END PutTextStylingControl;
		
	BEGIN
		out := TextModels.dir.New(); w.ConnectTo(out);
		f := Fonts.dir.Default(); tf := f.typeface;
		fnum := 1; fonts[0] := tf;
		cnum := 1; colors[0] := Ports.defaultColor;
		col := Ports.defaultColor; size := 12 * Ports.point;
		offs := 0; style := {}; weight := Fonts.normal;
		attr0 := NIL; rattr0 := NIL; firstLine := TRUE; firstLine0 := FALSE;
		paragraph := FALSE; lastHeadingSize := 0;
		text := in.ThisModel(); r := text.NewReader(NIL);
		ruler := TextViews.ThisRuler(in, 0 (*beg*)); rattr := ruler.style.attr;
		(*r.SetPos(beg);*) r.ReadChar(ch);
		attr := NIL;
		WHILE ~r.eot (*& (r.Pos() <= end)*) DO
			IF attr # r.attr THEN
				IF (attr = NIL) OR (r.attr = NIL) OR ~attr.Equals(r.attr) THEN
					(*Close(w, attr);*)
					attr := r.attr;
					(*Open(w, attr)*)
				END
			END;
			IF (r.view # NIL) & (r.view IS TextRulers.Ruler) THEN
				ruler := r.view(TextRulers.Ruler); rattr := ruler.style.attr;
				firstLine := TRUE
			ELSIF ch = FF THEN firstLine := TRUE
			END;
			(*
			IF (rattr # rattr0) OR (firstLine # firstLine0) THEN
				IF (rattr # rattr0) OR (rattr.first # rattr.left) OR
					(rattr.lead # 0) OR (TextRulers.pageBreak IN rattr.opts) THEN	PutTextRulersStyling(rattr);
				END;
				rattr0 := rattr; firstLine0 := firstLine
			END;
			*)
			IF firstLine THEN paragraph := FALSE END;
			IF attr # attr0 THEN
				PutTextStylingControl(attr);
				attr0 := attr
			END;
			PutChar(ch);
			r.ReadChar(ch)
		END;
		PutControlEx("fs", 0);
		(*Close(w, attr)*)
	END ConvertToRichText;
	
	PROCEDURE Exporter (s: Stores.Store; f: Files.File);
		VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
	BEGIN
		ASSERT(s # NIL, 20);
		(*ASSERT(f # NIL, 21); ASSERT(f.Length() = 0, 22);*) (* disabled for output without storing in files *)
		ASSERT(s IS TextViews.View, 23);
		s := TextView(s);
		IF s # NIL THEN
			ConvertToRichText(s(TextViews.View), exportOpts, t);
			Views.OpenView(TextViews.dir.New(t)); (* create a text view for the text generated above and open the text view in its own window *)
			(*
			w.ConnectTo(f); w.SetPos(0);
			t := s(TextViews.View).ThisModel();
			IF t # NIL THEN
				r := t.NewReader(NIL);
				r.ReadChar(ch);
				WHILE ~r.eot DO
					WriteChar (w, ch);
					r.ReadChar(ch)
				END
			END
			*)
		END
	END Exporter;
	
	PROCEDURE ExportDocument (options: SET);
		VAR
			v : Views.View;
			loc: Files.Locator;
			name : Files.Name;
			conv: Converters.Converter;
			doc : Files.File;
			res : INTEGER;
	BEGIN
		exportOpts := options;
		loc := NIL; name := ""; conv := NIL;
		v := Views.Old(Files.ask, loc, name, conv); (* load view from file via standard selection dialog *)
		IF (v # NIL) & (v IS TextViews.View) THEN
			(*doc := Files.dir.New(loc, FALSE);*) (* disabled: not using files *)
			Exporter(v(TextViews.View), NIL(*doc*));
		END;
	END ExportDocument;
	
	(** Export documentation *)
	PROCEDURE ExportDocu*;
	BEGIN
		ExportDocument( { preserveFormatting, preserveTabs } )
	END ExportDocu;
	
	(** Export source code *)
	PROCEDURE ExportMod*;
	BEGIN
		ExportDocument( { } )
	END ExportMod;

END ConvDokuWikiExporter.