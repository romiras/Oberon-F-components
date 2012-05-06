(*	$Id: IntStr.Mod,v 1.2 1997/06/17 07:56:17 acken Exp $	*)
MODULE OttIntStr;
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
(*  IntStr - Integer-number/string conversions.       
    Copyright (C) 1995 Michael Griebling
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
    Copyright (C) 1995 Michael Griebling
    	This file is part of OOC.
	------------------------------------
*)
  
IMPORT
  Conv := OttConvTypes, IntConv := OttIntConv
(*bbox:*), OttOSA
 ;
 
TYPE
  ConvResults*= Conv.ConvResults; 
  (* possible values: strAllRight, strOutOfRange, strWrongFormat, strEmpty *)

CONST
  strAllRight*=Conv.strAllRight;
  (* the string format is correct for the corresponding conversion *)
  strOutOfRange*=Conv.strOutOfRange;
  (* the string is well-formed but the value cannot be represented *)
  strWrongFormat*=Conv.strWrongFormat;
  (* the string is in the wrong format for the conversion *)
  strEmpty*=Conv.strEmpty;
  (* the given string is empty *)
 
 
(* the string form of a signed whole number is
     ["+" | "-"] decimal_digit {decimal_digit}
*)
 
PROCEDURE StrToInt*(str: ARRAY OF CHAR; VAR int: LONGINT; VAR res: ConvResults);
(* Ignores any leading spaces in `str'. If the subsequent characters in `str'
   are in the format of a signed whole number, assigns a corresponding value to
   `int'.  Assigns a value indicating the format of `str' to `res'.  *)
BEGIN
  res:=IntConv.FormatInt(str);
  IF (res = strAllRight) THEN
    int:=IntConv.ValueInt(str)
  END
END StrToInt;


PROCEDURE Reverse (VAR str : ARRAY OF CHAR; start, end : INTEGER);
(* Reverses order of characters in the interval [start..end]. *)
VAR
  h : CHAR;
BEGIN
  WHILE start < end DO
    h := str[start]; str[start] := str[end]; str[end] := h;
    INC(start); DEC(end)
  END
END Reverse;


PROCEDURE IntToStr*(int: LONGINT; VAR str: ARRAY OF CHAR);
(* Converts the value of `int' to string form and copies the possibly truncated
   result to `str'.  *)
CONST
  maxLength = 11; (* maximum number of digits representing a LONGINT value *)
VAR
  b : ARRAY maxLength+1 OF CHAR;
  s, e: INTEGER;
BEGIN
  (* build representation in string 'b' *)
  IF int = MIN(LONGINT) THEN      (* smallest LONGINT, -int is an overflow *)
    b := "-2147483648";
    e := 11
  ELSE
    IF int < 0 THEN               (* negative sign *)
      b[0] := "-"; int := -int; s := 1
    ELSE  (* no sign *)
      s := 0
    END;
    e := s;                       (* 's' holds starting position of string *)
    REPEAT
      b[e] := CHR(int MOD 10+ORD("0"));
      int := int DIV 10;
      INC(e)
    UNTIL int = 0;
    b[e] := 0X;
    Reverse(b, s, e-1)
  END;
  (*old: COPY(b, str) (* truncate output if necessary *) *)
OttOSA.COPY(b, str);
END IntToStr;
 
END OttIntStr.

