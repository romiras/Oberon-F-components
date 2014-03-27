MODULE ArcExportDoc;

(*
	Tool for export of Oberon/F documents into XHTML with views exported as PNG images. Custom version of XhtmlExporter is used.
	Processed all "Docu" subdirectories of all subsystems in BlackBox root directory.
*)

	IMPORT Strings, Files, Views, TextViews, Converters, XhtmlExporter, E := ArcEnumDocs, Out;
	
	TYPE
		String = E.String;
	
	VAR
		conv: Converters.Converter;
	
	PROCEDURE FindConverter;
	BEGIN
		conv := Converters.list;
		WHILE (conv # NIL) & (conv.exp # "XhtmlExporter.ExportText") DO conv := conv.next END
	END FindConverter;
	
	PROCEDURE Export (loc: Files.Locator; IN docname, expName: ARRAY OF CHAR): BOOLEAN;
		VAR v: Views.View;
	BEGIN
		v := Views.OldView(loc, docname$ + ".odc");
		IF (conv # NIL) & (v # NIL) & (v IS TextViews.View) THEN		(* make sure it is a text view *)
			XhtmlExporter.documentName := docname$;
			Converters.Export(Files.dir.This(""), expName$, conv, v);
			RETURN loc.res = 0
		ELSE
			RETURN FALSE
		END
	END Export;
	
	PROCEDURE ExportDoc (loc: Files.Locator; IN subsystem, name: ARRAY OF CHAR);
		VAR expName: String; path: Files.Name;
	BEGIN
		expName := name$ + ".html";
		IF (subsystem # "System") & (subsystem # "Docu") THEN
			expName := subsystem$ + expName$
		END;
		Out.String(name$ + " - "); 
		IF Export(loc, name$, expName$) THEN
			Out.String(expName$)
		ELSE
			Out.String("NOT exported!")
		END;
		Out.Ln
	END ExportDoc;
	
	PROCEDURE ExportList;
		VAR listHead: E.DocList;
	BEGIN
		listHead := E.listRoot;
		WHILE listHead # NIL DO
			ExportDoc(listHead.loc, listHead.subName$, listHead.docName$);
			listHead := listHead.next
		END
	END ExportList;
	
	PROCEDURE Do*;
		VAR loc: Files.Locator; li: Files.LocInfo;
	BEGIN
		loc := Files.dir.This(""); (* BlackBox's root directory *)
		E.ScanSubsystems (loc, FALSE, 0);
		ExportList;
	END Do;
	
BEGIN
	FindConverter
END ArcExportDoc.Do