(*	$Id: ConvTypes.Mod,v 1.1 1997/02/07 07:45:32 oberon1 Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
MODULE OttConvTypes;
  
(*  Common types used in the string conversion modules 
    Copyright (C) 1997, 1998  Michael van Acken
    Copyright (C) 1998  Ian Rae
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
	    Copyright (C) 1997, 1998  Michael van Acken
    	This file is part of OOC.
	------------------------------------
*)
 
TYPE
  ConvResults*= SHORTINT;    (* Values of this type are used to express the format of a string *)

CONST
  strAllRight*=0;    (* the string format is correct for the corresponding conversion *)
  strOutOfRange*=1;  (* the string is well-formed but the value cannot be represented *)
  strWrongFormat*=2; (* the string is in the wrong format for the conversion *)
  strEmpty*=3;       (* the given string is empty *)


TYPE
  ScanClass*= SHORTINT; (* Values of this type are used to classify input to finite state scanners *)
  
CONST
    padding*=0;    (* a leading or padding character at this point in the scan - ignore it *)
    valid*=1;      (* a valid character at this point in the scan - accept it *)
    invalid*=2;    (* an invalid character at this point in the scan - reject it *)
    terminator*=3; (* a terminating character at this point in the scan (not part of token) *)


TYPE
  ScanState*=POINTER TO ScanDesc; 
  ScanDesc*=  (* The type of lexical scanning control procedures *)
    RECORD
      p*: PROCEDURE (ch: CHAR; VAR cl: ScanClass; VAR st: ScanState);
    END;
 
END OttConvTypes.
