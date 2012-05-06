MODULE OttSample2;
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
IMPORT OttTextRider, Ch := OttChannel, Out := OttOut, OttFiles;

PROCEDURE Go*;
VAR chan1: Ch.Channel;  r: OttTextRider.Reader; res: INTEGER; n: INTEGER;
	sum: INTEGER; errbuf: ARRAY 80 OF CHAR;
BEGIN
	chan1 := OttFiles.Old("integers.txt", { OttFiles.read}, res);
	IF res # Ch.done THEN
		(* write error msg to default output stream.  Note that
			Out.SetWriter is not necessary *)
		OttFiles.ErrorDescr(res, errbuf);
		Out.String(errbuf); Out.Ln;
	ELSE
		r := OttTextRider.ConnectReader(chan1);
		IF r = NIL THEN
			chan1.ErrorDescr(errbuf);
			Out.String(errbuf); Out.Ln;
		END;
		sum := 0;
		r.ReadInt(n);
		WHILE (r.Res() = Ch.done) DO
			sum := sum + n;
			r.ReadInt(n);
		END;
	
		Out.String("The sum is "); Out.Int(sum, 6); Out.Ln;
	END;
	
END Go;

(* invoke
  OttSample2.Go  
  DevDebug.UnloadModuleList 
OttSample2 OttIn TextRider OttOut OttFiles
*)

END OttSample2.
