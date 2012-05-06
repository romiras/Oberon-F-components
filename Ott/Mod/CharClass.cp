(*	$Id: CharClass.Mod,v 1.3 1998/07/03 13:54:13 acken Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
MODULE OttCharClass;  
(*  Classification of values of the type CHAR 
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
 
(* 
Notes:
- This module boldly assumes ASCII character encoding. ;-)
- The value `eol' and the procedure `IsEOL' are not part of the Modula-2
  DIS.  OOC defines them to fixed values for all its implementations, 
  independent of the target system.  The string `systemEol' holds the target 
  system's end of line marker, which can be longer than one byte (but cannot
  contain 0X).
*)

IMPORT
  Ascii := OttAscii, Kernel;

CONST
  eol* = Ascii.lf;
  (* the implementation-defined character used to represent end of line
     internally for OOC  *)

VAR
  systemEol-: ARRAY 3 OF CHAR;
  (* End of line marker used by the target system for text files.  The string
     defined here can contain more than one character.  For one character eol
     markers, `systemEol' must not necessarily equal `eol'.  Note that the
     string cannot contain the termination character 0X.  *)


PROCEDURE IsNumeric* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch is classified as a numeric character *)
  BEGIN
    RETURN ("0" <= ch) & (ch <= "9")
  END IsNumeric;
  
PROCEDURE IsLetter* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch is classified as a letter *)
  BEGIN
    RETURN ("a" <= ch) & (ch <= "z") OR ("A" <= ch) & (ch <= "Z")
  END IsLetter;
  
PROCEDURE IsUpper* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch is classified as an upper case letter *)
  BEGIN
    RETURN ("A" <= ch) & (ch <= "Z")
  END IsUpper;
  
PROCEDURE IsLower* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch is classified as a lower case letter *)
  BEGIN
    RETURN ("a" <= ch) & (ch <= "z")
  END IsLower;
  
PROCEDURE IsControl* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch represents a control function *)
  BEGIN
    RETURN (ch < Ascii.sp)
  END IsControl;
  
PROCEDURE IsWhiteSpace* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch represents a space character or a format 
   effector *)
  BEGIN
    RETURN (ch = Ascii.sp) OR (ch = Ascii.ff) OR (ch = Ascii.lf) OR
           (ch = Ascii.cr) OR (ch = Ascii.ht) OR (ch = Ascii.vt)
  END IsWhiteSpace;

  
PROCEDURE IsEol* (ch: CHAR): BOOLEAN;
(* Returns TRUE if and only if ch is the implementation-defined character used
   to represent end of line internally for OOC.  *) 
  BEGIN
    RETURN (ch = eol)
  END IsEol;

BEGIN
IF Kernel.littleEndian THEN (* assume Intel, therefore Win32 *)
  systemEol[0] := Ascii.cr; systemEol[1] := Ascii.lf; systemEol[2] := 0X;
ELSE (* assume Mac *)
  systemEol[0] := Ascii.cr; systemEol[1] := 0X
END;
END OttCharClass.
