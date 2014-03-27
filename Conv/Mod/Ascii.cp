MODULE ConvAscii;

	IMPORT Files, Stores, Views, Properties, TextModels, TextViews;
	
	PROCEDURE ThisWndChar (ch: CHAR): SHORTCHAR;
	BEGIN
		IF ch = 2013X THEN ch := '-' 	(* EN DASH *)
		ELSIF (ch = 0BBX) OR (ch = 0ABX) THEN ch := '"' 	(* LEFT AND RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK *)
		ELSIF ch = 2026X THEN ch := "_"	(* HORIZONTAL ELLIPSIS *)
		END;
		RETURN SHORT(ch)
	END ThisWndChar;
	
	PROCEDURE WriteChar (w: Stores.Writer; ch: CHAR);
		VAR wch: SHORTCHAR;
	BEGIN
		IF (ch # TextModels.viewcode) & (ch # TextModels.para) THEN
			wch := ThisWndChar(ch);
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

	PROCEDURE ExportText* (s: Stores.Store; f: Files.File);
		VAR w: Stores.Writer; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
	BEGIN
		ASSERT(s # NIL, 20); ASSERT(f # NIL, 21); ASSERT(f.Length() = 0, 22);
		ASSERT(s IS TextViews.View, 23);
		s := TextView(s);
		IF s # NIL THEN
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
		END
	END ExportText;
	
END ConvAscii.