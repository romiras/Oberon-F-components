(*	$Id: Out.Mod,v 1.3 1997/10/11 20:01:28 acken Exp $	*)
MODULE OttOut;
(****TLIB keywords*** "%n %v %f" *)
(* "LREALC~1.MOD 1.2 22-Jul-98,05:07:22" *)
(*    Out -  Simple terminal output of Oberon variables.       
    Copyright (C) 1997 Michael Griebling
 	Copyright (C) 1998 Ian Rae
    This file is part of OTT.

    OTT is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.  

    OTT is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details. 

    You should have received a copy of the GNU General Public License
    along with OTT. If not, write to the Free Software Foundation, 59
    Temple Place - Suite 330, Boston, MA 02111-1307, USA.

	------------------------------------
	Note. OTT is a modified and extended version of OOC.  The original OOC
	 copyright for this file is:
	    Copyright (C) 1997 Michael Griebling
    	This file is part of OOC.
	------------------------------------
*)

IMPORT
  Log := OttLog, TextRider := OttTextRider;

VAR
  writer-: TextRider.Writer;
  (* This is the rider used by the output procedures.  Initialized to hold a
     text writer to the OttLog channel.  Can be changed by 
     calling SetWriter.  *)
     

PROCEDURE Done* () : BOOLEAN;
(* Returns TRUE if the last operation was successful.  *)
  BEGIN
    RETURN writer.Res() = TextRider.done
  END Done;

PROCEDURE ClearError*;
  BEGIN
    writer. ClearError
  END ClearError;


PROCEDURE SetWriter* (w: TextRider.Writer);
(* Changes the rider `writer'.  All following calls to output procedures will 
   write to `w'.  The preferred method of connecting to the standard output
   channel is with a call like SetWriter(NIL).  NOTE: If interactive input
   is desired, ensure that the writer output buffering is turned off as
   follows: 
   
        Out.writer.SetOpts({TextRider.noBuffering}) 
 *)
  VAR
	log: Log.Log;
	eol: ARRAY 3 OF CHAR;
  BEGIN
    IF w=NIL THEN 
	log := Log.Open();
	ASSERT(log# NIL);
	writer:=TextRider.ConnectWriter (log);     
	eol[0] := 0DX; eol[1] := 0X;
	writer.SetEol(eol, 1);
    ELSE writer:=w
    END
  END SetWriter;

PROCEDURE Flush*;
(* Flushes all buffers associated with `writer'.  *)
  BEGIN
    writer. base. Flush
  END Flush;


PROCEDURE Char* (ch: CHAR);
  BEGIN
    writer. WriteChar (ch)
  END Char;
  
PROCEDURE String* (s: ARRAY OF CHAR);
  BEGIN
    writer. WriteString (s)
  END String;
  
PROCEDURE Bool* (bool: BOOLEAN);
  BEGIN
    writer. WriteBool (bool)
  END Bool;
  
PROCEDURE LongInt* (lint: LONGINT; n: LONGINT);
  BEGIN
    writer. WriteLInt (lint, n)
  END LongInt;

PROCEDURE ShortInt* (sint: SHORTINT; n: LONGINT);
  BEGIN
    writer. WriteSInt (sint, n)
  END ShortInt;

PROCEDURE Int* (int: INTEGER; n: LONGINT);
  BEGIN
    writer. WriteInt (int, n)
  END Int;

PROCEDURE Hex* (lint: LONGINT; n: LONGINT);
  BEGIN
    writer. WriteHex (lint, n) 
  END Hex;
  
PROCEDURE LongReal* (lreal: TextRider.OttLONGREAL; n, k: LONGINT);
(* Write `lreal' with `k' significant digits and right-justified
   in a field of width n. *)
  BEGIN
    writer. WriteLReal (lreal, n, k)
  END LongReal;

PROCEDURE Real* (real: REAL; n, k: LONGINT);
(* as LongReal *)
  BEGIN
    writer. WriteReal (real, n, k)
  END Real;

PROCEDURE LongRealFix* (lreal: TextRider.OttLONGREAL; n, k: LONGINT);
(* Write `lreal' rounded to `k' digits relative to the decimal
   point and right-justified in a field of width n.  Negative
   values of `k' round to the left of the decimal point and
   positive `k' round to the right of the decimal point. *)
  BEGIN
    writer. WriteLRealFix (lreal, n, k)
  END LongRealFix;

PROCEDURE RealFix* (real: REAL; n, k: LONGINT);
(* as LongRealFix *)
  BEGIN
    writer. WriteRealFix (real, n, k)
  END RealFix;

PROCEDURE LongRealEng* (lreal: TextRider.OttLONGREAL; n, k: LONGINT);
(* as LongReal except exponent is always a multiple of
   3 and there are 1 to 3 digits to the left of the
   decimal point. *)
  BEGIN
    writer. WriteLRealEng (lreal, n, k)
  END LongRealEng;

PROCEDURE RealEng* (real: REAL; n, k: LONGINT);
(* as LongRealEng *)
  BEGIN
    writer. WriteRealEng (real, n, k)
  END RealEng;
  
PROCEDURE Set* (s: SET);
  BEGIN
    writer. WriteSet (s)
  END Set;

PROCEDURE Ln*;
  BEGIN
    writer. WriteLn
  END Ln;


BEGIN
  SetWriter(NIL) 
END OttOut.
