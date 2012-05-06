(*	$Id: LRealConv.Mod,v 1.4 1998/07/11 18:18:51 acken Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "LREALC~1.MOD 1.2 22-Jul-98,05:07:22" *)
MODULE OttLRealConv;
(*   LRealConv -  Low-level LONGREAL/string conversions.       
    Copyright (C) 1996 Michael Griebling
	Copyright (C) 1998 Ian Rae
    This file is part of OTT.

Bbox: heavily butchered to remove all but string parsing.

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
  Char := OttCharClass, (*Low := LowLReal,*)Str := Strings, Conv := OttConvTypes;
(*  LInt := LongInts;*)

CONST
  ZERO=0.0E0; (* was 0.0D0 *)
  SigFigs*=15;  (* accuracy of LONGREALs *)
  
  DEBUG = FALSE;
 
TYPE
  ConvResults*= Conv.ConvResults; (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
(*  LongInt=LInt.LongInt; *)

CONST
  strAllRight*=Conv.strAllRight;       (* the string format is correct for the corresponding conversion *)
  strOutOfRange*=Conv.strOutOfRange;   (* the string is well-formed but the value cannot be represented *)
  strWrongFormat*=Conv.strWrongFormat; (* the string is in the wrong format for the conversion *)
  strEmpty*=Conv.strEmpty;             (* the given string is empty *)

VAR
  RS, P, F, E, SE, WE, SR: Conv.ScanState;
  

PROCEDURE IsExponent (ch: CHAR) : BOOLEAN;
BEGIN
  RETURN (ch="E") OR (ch="D")
END IsExponent;

PROCEDURE IsSign (ch: CHAR): BOOLEAN;
(* Return TRUE for '+' or '-' *)
BEGIN
  RETURN (ch='+')OR(ch='-')
END IsSign;  


(* internal state machine procedures *)

PROCEDURE RSState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=P
  ELSE chClass:=Conv.invalid; nextState:=RS
  END
END RSState;
  
PROCEDURE PState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=P
  ELSIF inputCh="." THEN chClass:=Conv.valid; nextState:=F
  ELSIF IsExponent(inputCh) THEN chClass:=Conv.valid; nextState:=E  
  ELSE chClass:=Conv.terminator; nextState:=NIL
  END
END PState;
  
PROCEDURE FState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=F
  ELSIF IsExponent(inputCh) THEN chClass:=Conv.valid; nextState:=E  
  ELSE chClass:=Conv.terminator; nextState:=NIL
  END
END FState;
 
PROCEDURE EState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF IsSign(inputCh) THEN chClass:=Conv.valid; nextState:=SE
  ELSIF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=WE  
  ELSE chClass:=Conv.invalid; nextState:=E
  END
END EState;

PROCEDURE SEState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=WE  
  ELSE chClass:=Conv.invalid; nextState:=SE
  END
END SEState;

PROCEDURE WEState(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
BEGIN
  IF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=WE  
  ELSE chClass:=Conv.terminator; nextState:=NIL
  END
END WEState;

(* PROCEDURE Real (VAR x: LongInt; exp, digits: LONGINT): LONGREAL; -- removed *)

PROCEDURE ScanReal*(inputCh: CHAR; VAR chClass: Conv.ScanClass; VAR nextState: Conv.ScanState);
 (* 
    Represents the start state of a finite state scanner for real numbers - assigns
    class of inputCh to chClass and a procedure representing the next state to
    nextState.
    
    The call of ScanReal(inputCh,chClass,nextState) shall assign values to
    `chClass' and `nextState' depending upon the value of `inputCh' as
    shown in the following table.
    
    Procedure       inputCh         chClass         nextState (a procedure
                                                    with behaviour of)
    ---------       ---------       --------        ---------
    ScanReal        space           padding         ScanReal
                    sign            valid           RSState
                    decimal digit   valid           PState
                    other           invalid         ScanReal
    RSState         decimal digit   valid           PState
                    other           invalid         RSState
    PState          decimal digit   valid           PState
                    "."             valid           FState
                    "E", "D"        valid           EState
                    other           terminator      --
    FState          decimal digit   valid           FState
                    "E", "D"        valid           EState
                    other           terminator      --
    EState          sign            valid           SEState
                    decimal digit   valid           WEState
                    other           invalid         EState
    SEState         decimal digit   valid           WEState
                    other           invalid         SEState
    WEState         decimal digit   valid           WEState
                    other           terminator      --
   
    For examples of how to use ScanReal, refer to FormatReal and
    ValueReal below.     
  *)
BEGIN
  IF Char.IsWhiteSpace(inputCh) THEN chClass:=Conv.padding; nextState:=SR
  ELSIF IsSign(inputCh) THEN chClass:=Conv.valid; nextState:=RS
  ELSIF Char.IsNumeric(inputCh) THEN chClass:=Conv.valid; nextState:=P
  ELSE chClass:=Conv.invalid; nextState:=SR
  END
END ScanReal;
 
(* PROCEDURE FormatReal*(str: ARRAY OF CHAR): ConvResults; -- removed *)
 
(* PROCEDURE ValueReal*(str: ARRAY OF CHAR): LONGREAL; -- removed *)

(* PROCEDURE LengthFloatReal*(real: LONGREAL; sigFigs: INTEGER): INTEGER; -- removed *)

(* PROCEDURE LengthEngReal*(real: LONGREAL; sigFigs: INTEGER): INTEGER; -- removed *)

(* PROCEDURE LengthFixedReal*(real: LONGREAL; place: INTEGER): INTEGER; -- removed *)

(* PROCEDURE IsRConvException*(): BOOLEAN; -- removed *)


BEGIN
  NEW(RS); NEW(P); NEW(F); NEW(E); NEW(SE); NEW(WE); NEW(SR);
  RS.p:=RSState; P.p:=PState; F.p:=FState; E.p:=EState;
  SE.p:=SEState; WE.p:=WEState; SR.p:=ScanReal;
(*  IF DEBUG THEN Test END *)
END OttLRealConv.
