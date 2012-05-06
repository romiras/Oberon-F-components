MODULE OttTokenizer ; (* [FOREIGN "C"; LINK FILE "Files.c" END]; <* Warnings := FALSE *> *)
(****TLIB keywords*** "%n %v %f" *)
(* "CHANNEL.MOD 1.2 22-Jul-98,04:55:52" *)
(*  Simple string parsing
    Copyright (C) 1998  Ian Rae

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
	Note. OTT is a modified and extended version of OOC.  This file is
	however unique to OTT.
	------------------------------------
*)


IMPORT
   Ch := OttChannel, OttTextRider,  OttAscii, OttOSA
(* testing:	,Log := StdLog *)
  ;
  
(* CONST here *)
CONST
  done* = Ch.done;
  bufTooSmall* = 999; (* any value that doesn't collide with Channel and TextRider errors *)

(*--- concrete File ---*)
TYPE
  Reader* = POINTER TO ReaderDesc;
  ReaderDesc* = EXTENSIBLE RECORD
	res-: INTEGER;		 (* shadows reader.res, and has a few Tokenizer-specific errors *)
	delims-: ARRAY 256+1 OF CHAR; (* delimiter chars. must be at least 1. Max 256. *)
	reader-: OttTextRider.Reader; (* used to read base *)
  END;

(*===========================================================*)
PROCEDURE ConnectReader*(ch: Ch.Channel): Reader;
  VAR
    r: Reader;
  BEGIN
	IF ch = NIL THEN
		RETURN NIL;
	END;
    NEW(r);
	(* default delims are the whitespace chars *)
	r.delims[0] := OttAscii.sp;
	r.delims[1] := OttAscii.ff;
	r.delims[2] := OttAscii.lf;
	r.delims[3] := OttAscii.cr;
	r.delims[4] := OttAscii.ht;
	r.delims[5] := OttAscii.vt;
	r.delims[6] := 0X;
	
	r.reader := OttTextRider.ConnectReader(ch);
	r.res := r.reader.Res(); (* sync our res with reader's res *)
	IF r.reader = NIL THEN
		RETURN NIL
	ELSE
		RETURN r;
	END;
  END ConnectReader;

(*===========================================================*)
PROCEDURE IsDelim(r: Reader; x: CHAR) : BOOLEAN;
(*  Determines if x is a delimter char.  Does not change the
  underlying reader at all.
	pre: x is some non-0X char
	post: x in delims[] and return is TRUE
           or x not in delims[] and return is FALSE
*)
VAR
	i: INTEGER;
BEGIN
	ASSERT(r.reader # NIL);
	i := 0;
     WHILE (r.delims[i] # 0X) DO
		IF x = r.delims[i]  THEN RETURN TRUE END;
		INC (i)
    END;
	RETURN FALSE;
END IsDelim;

(*===========================================================*)
PROCEDURE (r: Reader) SkipDelims*(VAR x: CHAR), NEW;
(* Advances reader until eob or a non-delim char is encountered or an error
	is encountered in the reader.
	pre: none
	post: no reader error and x is 1st non-delim char encountered
	     or reader error and ch is 0X or last char read by this routine.
	     if last char is non-delim then res is 0 and x is char. (readAfterEnd means attempted
	     to do read when past eob)
*)
VAR
BEGIN
ASSERT(r.reader # NIL);
x := 0X;
REPEAT
	r.reader.ReadChar(x);
UNTIL (r.reader.Res() # done) OR (~IsDelim(r, x));
r.res := r.reader.Res(); (* sync our res with reader's res *)
END SkipDelims;
  
(*===========================================================*)
PROCEDURE (r: Reader) GetToken*(VAR buf: ARRAY OF CHAR), NEW;
(* Gets the next token from the connected channel.  First advances past
   any delimiter chars then copies non-delim chars into buf until a delim char
   is encountered again.  Error if buf too small.
   reader errors include end-of-buffer and any other errors.
  pre: none
  post: no reader error and buf is next token and reader at first delim char after token
	     or reader error and buf is ""
	     or buf too small and buf contains truncated token and res is bufTooSmall and reader at wherever
	     buf filled up.
*)
VAR i: INTEGER;
	x: CHAR; maxLen: INTEGER;
BEGIN
ASSERT(r.reader # NIL);
buf[0] := 0X;
r.SkipDelims(x);
IF r.res # done THEN 
	(* not an error if last char is a token. res will be readAfterEnd *)
	IF (r.res = Ch.readAfterEnd) & (LEN(buf) > 1) THEN
		buf[0] := x; buf[1] := 0X;
	END;
	RETURN 
END;
i := 0; maxLen := LEN(buf) - 1; (* -1 to leave room for 0X *)
REPEAT
	IF i >= maxLen THEN
		r.res := bufTooSmall;
		RETURN;
	END;
	buf[i] := x; INC(i);
	r.reader.ReadChar(x);
UNTIL (r.reader.Res() # done) OR (IsDelim(r,x));
buf[i] := 0X;
(* if at eob then leave res = done since buf contains final token.  Next
   call to GetToken() will return readAfterEnd.  If we're here then SkipDelims()
	didn't encounter an error so must be at least one char.  !!This may change
	with new TextRider version that fixes reading last char bug *)
r.res := r.reader.Res(); (* sync our res with reader's res *)
IF (r.res = Ch.readAfterEnd) THEN
	r.res := done;
END;
END GetToken;

(*===========================================================*)
PROCEDURE (r: Reader) SetDelims*(buf: ARRAY OF CHAR), NEW;
(* Set the delimiter chars to contents of buf.  If this fn is never called then
	the default delims are whitespace chars.
  pre: buf not empty
  post: delim chars equal content of buf
*)
BEGIN
IF LEN(buf) > 0 THEN
	OttOSA.COPY(buf, r.delims);
END
END SetDelims;

END OttTokenizer.
