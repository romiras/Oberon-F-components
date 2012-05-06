MODULE OttSample1;
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)

IMPORT OttIn, OttOut, OttUtils;

(* invoke this function with an input stream "34 45"
   For example on Bbox, select the string of numbers (not including
   the quote characters) and invoke OttSample1.Go *)
PROCEDURE Go*;
VAR n: INTEGER;
buf: ARRAY 120 OF CHAR;
BEGIN
	OttOut.SetWriter(NIL); (* open Out to default output stream *)
	OttOut.String("Hello, world"); OttOut.Ln;
	
	OttIn.SetReader(NIL); (* open In to default input stream *)
	IF OttIn.Done() THEN  (* always check for success since default input stream
									may not always be available *)
		OttIn.Int(n);
		IF OttIn.Done() THEN 
			OttOut.String("first integer is "); OttOut.Int(n, 6); OttOut.Ln;									
		END;			
		OttIn.Int(n);
		IF OttIn.Done() THEN
			OttOut.String("second integer is "); OttOut.Int(n, 6); OttOut.Ln;									
		END;			
	END
END Go;

(* invoke
 OttSample1.Go   34 45 56 
 DevDebug.UnloadModuleList 
OttSample1 OttIn OttUtils OttOut OttTextRider OttIntStr OttOSA
*)

END OttSample1.
