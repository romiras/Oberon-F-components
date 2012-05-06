(*	$Id: In.Mod,v 1.2 1997/10/11 20:01:06 acken Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
MODULE OttIn;
(*  In -  Simple terminal input of Oberon variables.       
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
  Texts := OttTexts, TextRider := OttTextRider;
    
VAR
  reader-: TextRider.Reader;
  (* This is the rider used by the input procedures.  Initialized to hold a
     text reader on the `StdChannels.stdin' channel.  Can be changed by 
     calling SetReader.  *)

PROCEDURE Done* () : BOOLEAN;
(* Returns TRUE if the last operation was successful.  *)
  BEGIN
	IF reader = NIL THEN
		RETURN FALSE;
	ELSE
	    RETURN reader. Res()=TextRider.done
	END;
  END Done;

PROCEDURE ClearError*;
  BEGIN
    reader. ClearError
  END ClearError;


PROCEDURE SetReader* (r: TextRider.Reader);
(* Changes the rider `reader'.  All following calls to input procedures will 
   write to `r'.  *)
VAR
	res: INTEGER;
	text: Texts.Text;
  BEGIN
    IF r=NIL THEN 
		text := Texts.FocusSelection(res);
		IF (text = NIL)OR (res # Texts.done) THEN (* no focus text? *)
			reader := NIL; (* next operation will trap unless.  User should check Done after this!!*)
		ELSE
			reader:=TextRider.ConnectReader(text)
		END
    ELSE reader := r
    END
  END SetReader;

PROCEDURE Char* (VAR ch: CHAR);
  BEGIN
    reader. ReadChar (ch);
  END Char;

PROCEDURE Line* (VAR s: ARRAY OF CHAR);
  BEGIN
    reader. ReadLine (s)   
  END Line;

PROCEDURE String* (VAR s: ARRAY OF CHAR);
  BEGIN
    reader. ReadString (s)   
  END String;  
  
PROCEDURE Identifier* (VAR s: ARRAY OF CHAR);
  BEGIN
    reader. ReadIdentifier (s) 
  END Identifier;
  
PROCEDURE Bool* (VAR bool: BOOLEAN);
  BEGIN
    reader. ReadBool (bool)   
  END Bool;
  
PROCEDURE ShortInt* (VAR int: SHORTINT);
  BEGIN
    reader. ReadSInt (int)   
  END ShortInt;
  
PROCEDURE Int* (VAR int: INTEGER);
  BEGIN
    reader. ReadInt (int);  
  END Int;

PROCEDURE LongInt* (VAR lint: LONGINT);
  BEGIN
    reader. ReadLInt (lint)  
  END LongInt;

PROCEDURE Hex* (VAR lint: LONGINT);
  BEGIN
    reader. ReadHex (lint) 
  END Hex;
  
PROCEDURE LongReal* (VAR lreal: TextRider.OttLONGREAL);
  BEGIN
    reader. ReadLReal (lreal)    
  END LongReal;

PROCEDURE Real* (VAR real: REAL);
  BEGIN
    reader. ReadReal (real)   
  END Real;
  
PROCEDURE Set* (VAR s: SET);
  BEGIN
    reader. ReadSet (s)    
  END Set;

BEGIN
  SetReader(NIL)
END OttIn.




