MODULE OttSample3;
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
(* Counts characters, words, and lines in the focus text, similar 
to the UNIX command wc 
*)
IMPORT OttTexts, Out := OttOut, OttCharClass, OttChannel, OttAscii;

PROCEDURE IsEOL(r: OttChannel.Reader; prevCh, ch: CHAR) : BOOLEAN;
(* handles channel-specific eol chars -- Bbox texts have cr as
EOL char 
In this example we know r.base is an OttText but this routine will work if it's
changed to an OttFile.
*)
VAR b: BOOLEAN;
BEGIN
WITH r: OttTexts.Reader DO 
	b := (ch = OttAscii.cr);
ELSE 
	IF OttCharClass.systemEol[1] = 0X THEN (* single eol char *)
		b := (ch = OttCharClass.systemEol[0]);
	ELSE
		b := (prevCh = OttCharClass.systemEol[0]);
		IF b THEN
			b := (ch = OttCharClass.systemEol[1]);
		END;
	END
END;
RETURN b;
END IsEOL;

PROCEDURE wc*;
VAR  text: OttTexts.Text;  r: OttTexts.Reader; res: INTEGER; ch, prevCh: CHAR;
	errbuf: ARRAY 80 OF CHAR;
	chars, words, lines: INTEGER; inWord: BOOLEAN;
BEGIN
	text := OttTexts.FocusText(res);
	IF res # OttTexts.done THEN
		(* write error msg to default output stream.  Note that
			Out.SetWriter is not necessary *)
		OttTexts.ErrorDescr(res, errbuf);
		Out.String(errbuf); Out.Ln;
	ELSE
		r := text.NewReader();
		IF r = NIL THEN
			text.ErrorDescr(errbuf);
			Out.String(errbuf); Out.Ln;
		END;
		chars := 0; words := 0; lines := 0;
		inWord := FALSE; prevCh := 0X;
		r.ReadByte(ch);
		WHILE (r.res = OttTexts.done) DO
			INC(chars);
			IF (IsEOL(r, prevCh, ch)) THEN INC(lines) END;
			IF inWord THEN
				IF OttCharClass.IsWhiteSpace(ch) THEN
					inWord := FALSE;
				END;
			ELSE
				IF ~OttCharClass.IsWhiteSpace(ch) THEN
					inWord := TRUE;
					INC(words);
				END
			END;
			prevCh := ch;
			r.ReadByte(ch);
		END;
	
		Out.String("wc: "); Out.Int(chars, 6); Out.Char(' '); 
		Out.Int(words, 6); Out.Char(' '); Out.Int(lines, 6); Out.Ln;
	END;
	
END wc;

(* invoke
 OttSample3.wc 
 DevDebug.UnloadModuleList 
OttSample3 OttIn TextRider OttOut OttFiles
*)

END OttSample3.
