MODULE ConvAsciiExporter;

	IMPORT Files, Views, TextViews, ConvAscii;
	
	PROCEDURE PathToFileSpec (IN path: ARRAY OF CHAR; OUT loc: Files.Locator; OUT name: Files.Name);
		VAR i, j: INTEGER; ch: CHAR;
	BEGIN
		i := 0; j := 0; loc := Files.dir.This("");
		WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
			ch := path[i]; INC(i);
			IF (j > 0) & (ch = "/") THEN name[j] := 0X; j := 0; loc := loc.This(name)
			ELSE name[j] := ch; INC(j)
			END
		END;
		IF path[i] = 0X THEN name[j] := 0X ELSE loc.res := 1; name := "" END
	END PathToFileSpec;
	
	PROCEDURE Exported (loc: Files.Locator; name : Files.Name; IN fullPath: ARRAY OF SHORTCHAR): BOOLEAN;
		VAR
			v : Views.View;
			doc : Files.File;
			res : INTEGER;
	BEGIN
		v := Views.OldView(loc, name);
		IF (v # NIL) & (v IS TextViews.View) THEN
			doc := Files.dir.New(loc, FALSE);
			ConvAscii.ExportText(v(TextViews.View), doc);
			doc.Register(fullPath$, "", Files.dontAsk, res);
			RETURN res = 0
		ELSE
			RETURN FALSE
		END;
	END Exported;
	
	PROCEDURE ExportDoc* (IN fileName : ARRAY OF SHORTCHAR; VAR tmpName : ARRAY OF SHORTCHAR) : BOOLEAN;
		VAR
			loc : Files.Locator;
			name : Files.Name;
			path: ARRAY 260 OF CHAR;
	BEGIN
		path := LONG(fileName$);
		PathToFileSpec(path, loc, name);
		tmpName := SHORT(name$) + ".tmp";
		RETURN Exported(loc, name, tmpName)
	END ExportDoc;
	
END ConvAsciiExporter.