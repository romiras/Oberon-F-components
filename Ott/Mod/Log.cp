(*	$Id: Logs.Mod,v 1.5 1997/06/20 14:11:42 acken Exp $	*)
MODULE OttLog ; (* [FOREIGN "C"; LINK Log "Logs.c" END]; <* Warnings := FALSE *> *)
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
(*  Access to Oberon environment log
    Copyright (C) 1997  Michael van Acken
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
	    Copyright (C) 1997  Michael van Acken
    	This file is part of OOC.
	------------------------------------
*)

(*<* ConformantMode := FALSE *>  *)(* for NewReader/NewWriter *)


IMPORT
   Ch := OttChannel, StdLog, Strings, OttAscii, OttOSA;
  
(* CONST here *)
CONST  (* NOTE: refer to module Channel for the meaning of the various codes *)
	padChar = ' '; (* char to pad texts when write past eot.  *)

CONST  (* NOTE: refer to module Channel for the meaning of the various codes *)
  noLength* = Ch.noLength;
  noPosition* = Ch.noPosition;
  
  (* the following values may appear in the `res' field of `Channel', `Reader',
     or `Writer': *)
  done* = Ch.done;
  invalidChannel* = Ch.invalidChannel;
  writeError* = Ch.writeError;
  noRoom* = Ch.noRoom; 

  (* symbolic values for `Reader.res' resp. `Writer.res': *)
  outOfRange* = Ch.outOfRange;
  channelClosed* = Ch.channelClosed;
  invalidFormat* = Ch.invalidFormat;
  
  (* symbolic values for `Ch.res': *)
  noReadAccess* = Ch.noReadAccess;
  noWriteAccess* = Ch.noWriteAccess;
     
  
CONST

(*--- concrete Log ---*)
TYPE
  Log* = POINTER TO LogDesc;
  LogDesc* = EXTENSIBLE RECORD
    (Ch.ChannelDesc)
	eol: CHAR; (* channel specific eol char.  StdLog uses Texts which use 'cr' *)
  END;

TYPE
  Writer* = POINTER TO WriterDesc;
  WriterDesc* = EXTENSIBLE RECORD 
    (Ch.WriterDesc)
  END;

(*===========================================================*)
PROCEDURE ErrorDescr* (res: INTEGER; VAR descr: ARRAY OF CHAR); 
(* Translates this module's error codes into strings.  The string starts with
   a capital letter and does not include any termination punctuation.  `descr'
   should be large enough to hold a multi-line message (256 characters should 
   suffice).
   If `res=done', then `descr' is assigned the empty string.  Note: You should
   use the type-bound ErrorDescr procedures instead whenever possible.  *)
  VAR
    str: ARRAY 128 OF CHAR;
  BEGIN
    CASE res OF
    ELSE
	Ch.ErrorDescr(res, str)
    END;
    OttOSA.COPY (str, descr) 
END ErrorDescr;
  

(*===========================================================*)
(* type-bound procedures from Channel.Reader:
   Pos, Available, SetPos, ReadByte, ReadBytes, ErrorDescr, ClearError *)


(*****************************************************************************************)
(*=========== Log Methods ==================================*)
(*****************************************************************************************)
(*===========================================================*)
(* type-bound procedures from Channel.Channel:
   NewWriter, Flush, Close, ErrorDescr *)
PROCEDURE (f: Log) Length*(): LONGINT, EXTENSIBLE;
(* Result is the number of bytes of data that this channel refers to.  
    Return length of underlying TextModel. *)
  BEGIN
	RETURN StdLog.text.Length(); (* return len of log's underlying TextModel *)
  END Length;
  
PROCEDURE (f: Log) GetModTime* (VAR mtime: INTEGER (*Time.TimeStamp*));
(* Retrieves the modification time of the data accessed by the given channel.
  Logs have no notion of time. *)
  BEGIN
	mtime := 0; (* do later *)
	f.res := (Ch.noModTime);
  END GetModTime;

PROCEDURE (f: Log) NewReader*(): Ch.Reader, EXTENSIBLE; 
(* Attaches a new reader to the Log `f'.  Log channels are output
 only.  NIL is returned and res is set to noReadAccess  *)
BEGIN 
	f.res := (Ch.noReadAccess);
	RETURN NIL;
END NewReader;

PROCEDURE (f: Log) NewWriter*(): Writer, EXTENSIBLE;
(* Attaches a new writer to the Log `f'.  It is placed at the very end
   of the Log, and its `res' field is initialized to `done'.  Note this
   is different than most channel writers which are initially positioned
   at the start. *)
VAR w: Writer;
BEGIN 
	NEW(w);

	(* update inherited fields *)	
	w.base :=(f);
	w.res := (Ch.done);
	w.bytesWritten := (0);
	w.positionable:= (FALSE); (* logs are not positionable *)
	
	f.res := (Ch.done);
	RETURN w;
END NewWriter;

PROCEDURE (f: Log) Flush*, EXTENSIBLE;
(* Flushes all buffers related to this channel.  Logs have no notion
    of flushing -- this function does nothing.
*)
  BEGIN
  END Flush;

PROCEDURE (f: Log) Close*;
(* All log objects point to the same Bbox log so close has no
	real effect.  Nothing is done. *)
BEGIN 
f.open:= (FALSE);
END Close;
  
PROCEDURE (f: Log) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why the
   previous operation (NewReader, NewWriter, Flush, Close, etc.) failed.  The 
   string starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `r.res = done', then `descr' is assigned the empty string.  *)
  BEGIN
    ErrorDescr (f. res, descr)
  END ErrorDescr;

(* ClearError inherited??!! *)


(*===========================================================*)
(* type-bound procedures from Channel.Writer:
   Pos, SetPos, WriteByte, WriteBytes, ErrorDescr, ClearError *)

PROCEDURE (w: Writer) Pos*(): LONGINT;
(* Returns the current writing position associated with the writer `w' in
   channel `w.base'.
  This procedure will return `noPosition' since logs are not positionable. *)
  BEGIN
	RETURN noPosition;
  END Pos;
  

PROCEDURE (w: Writer) SetPos* (newPos: LONGINT), EXTENSIBLE;
(* Sets the writing position to `newPos'.  A negative value of `newPos' or 
   calling this procedure for a writer that doesn't allow positioning will set
   `w.res' to `outOfRange'.  
	Logs are not positionable so sets w.res to outOfRange. 
   Calls to this procedure while `w.res # done' will be ignored.  *)
  BEGIN
    IF (w. res = Ch.done) THEN
        w. res :=  (Ch.outOfRange)
   ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed)
    END
  END SetPos;

PROCEDURE (w: Writer) WriteByte* (x: CHAR), EXTENSIBLE;
(* Writes a single byte `x' to the channel `w.base' at the writing position 
   associated with `w'.  The writing position is moved forward by one byte on 
   success, otherwise `w.res' is changed to indicate the error cause.
   `w.bytesWritten' will be 1 on success and 0 on failure.
   Calls to this procedure while `w.res # done' will be ignored.  
   If this byte is the channel's eol char, then Log.Ln is called. *)
  BEGIN
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
		IF x = w.base(Log).eol THEN
			StdLog.Ln
		ELSE
			StdLog.Char(x);
		END
      ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed);
        w. bytesWritten :=  (0)
      END
    ELSE
      w. bytesWritten :=  (0)
    END
  END WriteByte;

PROCEDURE (w: Writer) WriteBytes* (VAR x: ARRAY OF CHAR; start, n: LONGINT), EXTENSIBLE;
(* Writes `n' bytes from `x', starting at position `start', to the channel 
   `w.base' at the writing position associated with `w'.  The writing position
   is moved forward by `n' bytes on success, otherwise `w.res' is changed to 
   indicate the error cause.  `w.bytesWritten' will hold the number of bytes 
   that were actually written (being equal to `n' on success).
   Calls to this procedure while `w.res # done' will be ignored.
   pre: (n >= 0) & (0 <= start) & (start+n <= LEN (x))  *)
	VAR 
		i:INTEGER; pos: INTEGER; pat: ARRAY 2 OF CHAR;
  BEGIN                                  
    ASSERT ((n >= 0) & (0 <= start) & (start+n <= LEN (x)));
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN

	(* optimization: call StdLog.String if no eol chars in x *)
	pat[0] := w.base(Log).eol; pat[1] := 0X;
	Strings.Find(x, pat, 0, pos);
	IF (pos < 0) THEN (* no eol? *)
		StdLog.String(x);
	ELSE
		FOR i := 0 TO (SHORT(n) - 1) DO
			w.WriteByte(x[SHORT(start) + i]);
		END
	END;
	w.bytesWritten := (n);  
      ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed);
        w. bytesWritten :=(0)
      END
    ELSE
      w. bytesWritten := (0)
    END
  END WriteBytes;

PROCEDURE (w: Writer) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why one of
   the previous operations (SetPos, WriteByte, or WriteBytes) failed.  The
   string starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `w.res = done', then `descr' is assigned the empty string.  *)
  BEGIN
    ErrorDescr (w. res, descr)
  END ErrorDescr;


  
(*****************************************************************************************)
(*============== Module Fns ==================================*)
(*****************************************************************************************)
(* Log locator fn *)
  
PROCEDURE Open*(): Log;
(* Opens a connection to the log. Always succeeds *)
VAR f: Log;
BEGIN 
	NEW(f);
	f.eol := OttAscii.cr;
	(* set channel fields *)
	f.res :=  (Ch.done);
	f.readable :=(FALSE);
	f.writable:=( TRUE);
	f.open :=(TRUE);
	RETURN f
END Open;
    
END OttLog.
