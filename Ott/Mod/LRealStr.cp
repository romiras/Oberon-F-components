(*	$Id: LRealStr.Mod,v 1.5 1998/07/11 18:19:01 acken Exp $	*)
MODULE OttLRealStr;
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
(*      LRealStr -  LONGREAL/string conversions.       
    Copyright (C) 1996 Michael Griebling
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
	    Copyright (C) 1996 Michael Griebling
    	This file is part of OOC.
	------------------------------------
*)
 
IMPORT
  Conv := OttConvTypes, RC := OttLRealConv, Str := OttStrings, BBoxStrings := Strings;
 
CONST
  
TYPE
  ConvResults*= Conv.ConvResults; (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

CONST
  strAllRight*=Conv.strAllRight;       (* the string format is correct for the corresponding conversion *)
  strOutOfRange*=Conv.strOutOfRange;   (* the string is well-formed but the value cannot be represented *)
  strWrongFormat*=Conv.strWrongFormat; (* the string is in the wrong format for the conversion *)
  strEmpty*=Conv.strEmpty;             (* the given string is empty *)
 
 
(* the string form of a signed fixed-point real number is
     ["+" | "-"], decimal digit, {decimal digit}, [".", {decimal digit}]
*)
 
(* the string form of a signed floating-point real number is
     signed fixed-point real number, "E"|"e", ["+" | "-"], decimal digit, {decimal digit}
*)
 
PROCEDURE StrToReal*(str: ARRAY OF CHAR; VAR real: REAL; VAR res: ConvResults);
 (* 
    Ignores any leading spaces in str. If the subsequent characters in str 
    are in the format of a signed real number, and shall assign values to 
    `res' and `real' as follows:

    strAllRight  
      if the remainder of `str' represents a complete signed real number
      in the range of the type of `real' -- the value of this number shall
      be assigned to `real';
    
    strOutOfRange
      if the remainder of `str' represents a complete signed real number
      but its value is out of the range of the type of `real' -- the
      maximum or minimum value of the type of `real' shall be assigned to 
      `real' according to the sign of the number;
    
    strWrongFormat
      if there are remaining characters in `str' but these are not in the
      form of a complete signed real number -- the value of `real' is not
      defined;
    
    strEmpty
      if there are no remaining characters in `str' -- the value of `real'
      is not defined.    
  *)
VAR bboxRes: INTEGER;
BEGIN
(*old:  res:=RC.FormatReal(str);
  IF res IN {strAllRight, strOutOfRange} THEN real:=RC.ValueReal(str) END 
*)
	BBoxStrings.StringToReal(str, real, bboxRes);
     IF bboxRes=1 THEN res := strOutOfRange;
    ELSIF bboxRes=2 THEN  res := strWrongFormat;
    ELSE res := strAllRight;
    END;
END StrToReal;

(* all other routines removed for OTT *)

END OttLRealStr.




