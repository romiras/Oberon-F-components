(*Adapted from files.mod	$Id: Files.Mod,v 1.5 1997/06/20 14:11:42 acken Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHANNEL.MOD 1.2 22-Jul-98,04:55:52" *)
MODULE OttTexts ; (* [FOREIGN "C"; LINK FILE "Files.c" END]; <* Warnings := FALSE *> *)
(*  Access to target Oberon text object
    Copyright (C) 1998 Ian Rae

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

(* 
Note 1:
This text describes only the additional features of files beyond the standard
channels.  Please refer to Channel.Mod for the semantics of all items that are
inherited from Channel without modifications.

Note 2:
Most Unix systems only allow a fixed number of files (and sockets) to
be open simultaneously.  If this limit is reached, no new file can be
opened or socket be created until an old file/socket is closed.  For
any POSIX compliant system at least 16 open files are supported, most
implementations provide a much larger number.
*)
(*<* ConformantMode := FALSE *>  *)(* for NewReader/NewWriter *)

IMPORT
   Ch := OttChannel, (*Log := StdLog,*) TextModels, TextControllers, OttOSA;
  
(* CONST here *)
CONST  (* NOTE: refer to module Channel for the meaning of the various codes *)
	padChar = 20X; (* char to pad texts when write past eot.  Can't use 0X as per Channel,
							because Texts can't embed 0X bytes *)

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
  readAfterEnd* = Ch.readAfterEnd;
  channelClosed* = Ch.channelClosed;
  readError* = Ch.readError;
  invalidFormat* = Ch.invalidFormat;
  
  (* symbolic values for `Ch.res': *)
  noReadAccess* = Ch.noReadAccess;
  noWriteAccess* = Ch.noWriteAccess;
  closeError* = Ch.closeError;
  noModTime* = Ch.noModTime;
  noTmpName* = Ch.noTmpName;

(* these values report problems when creating a text *)
  noFocusText* = Ch.freeErrorCode;
  noFocusSelection* = Ch.freeErrorCode + 1;
  noInsertMode* = Ch.freeErrorCode + 2;
  noText* = Ch.freeErrorCode+3;

(*--- abstract Text ---*)
TYPE
  Text* = POINTER TO TextDesc;
  TextDesc* = EXTENSIBLE RECORD
    (Ch.ChannelDesc)
	txtmdl-: TextModels.Model;
  END;

TYPE
  Reader* =  POINTER TO ReaderDesc;
  ReaderDesc* = EXTENSIBLE RECORD
    (Ch.ReaderDesc)
	txtrdr:	TextModels.Reader;
	offset:    INTEGER;  (* 0 .. (end - beg) *)
  END;

TYPE
  Writer* = POINTER TO WriterDesc;
  WriterDesc* = EXTENSIBLE RECORD 
    (Ch.WriterDesc)
	txtwriter:	TextModels.Writer;
	offset:    INTEGER;  (* 0 .. (end - beg) *)
	insertMode-: BOOLEAN; (* ins/overwrite *)
  END;

(*--- FullText ---*)
TYPE
  FullText* = POINTER TO FullTextDesc;
  FullTextDesc* = EXTENSIBLE RECORD
    (TextDesc)
  END;

TYPE
  FullReader* = POINTER TO FullReaderDesc;
  FullReaderDesc* = EXTENSIBLE RECORD
    (ReaderDesc)
  END;

TYPE
  FullWriter* = POINTER TO FullWriterDesc;
  FullWriterDesc* = EXTENSIBLE RECORD 
    (WriterDesc)
  END;


(*--- SubText ---*)
(* derive from FullText so can inherit some methods *)
TYPE
  SubText* = POINTER TO SubTextDesc;
  SubTextDesc* = RECORD
    (FullTextDesc)
	beg, end:    INTEGER;  (* 0 .. (end - beg) *)
  END;

TYPE
  SubReader* = POINTER TO SubReaderDesc;
  SubReaderDesc* = RECORD
    (FullReaderDesc)
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
    | noFocusText:   str := "No text view has the focus"
    | noFocusSelection:   str := "No selection in focus text view"
    ELSE
	Ch.ErrorDescr(res, str)
    END;
    OttOSA.COPY(str, descr);
END ErrorDescr;
  

(*===========================================================*)
(* type-bound procedures from Channel.Reader:
   Pos, Available, SetPos, ReadByte, ReadBytes, ErrorDescr, ClearError *)

PROCEDURE (r: Reader) AssertValid(), NEW, EXTENSIBLE;  (* private *)
(* check for invariant *)
BEGIN
ASSERT(r.offset >= 0, 100);
(* can't check against underlying text model because it can be changed at
any time by other clients *)
ASSERT(r.txtrdr # NIL, 100);
END AssertValid;

PROCEDURE (w: Writer) AssertValid(), NEW, EXTENSIBLE;  (* private *)
(* check for invariant *)
BEGIN
ASSERT(w.offset >= 0, 100);
(* can't check against underlying text model because it can be changed at
any time by other clients *)
ASSERT(w.txtwriter # NIL, 100);
END AssertValid;

(*===========================================================*)
(* type-bound procedures from Channel.Channel:
   NewWriter, Flush, Close, ErrorDescr *)
PROCEDURE (T: Text) AssertValid(), NEW;  (* private *)
(* check for invariant *)
BEGIN
(* can't check against underlying text model because it can be changed at
any time by other clients *)
ASSERT(T.txtmdl # NIL, 100);
END AssertValid;

PROCEDURE (T: Text) GetModTime* (VAR mtime: INTEGER (*Time.TimeStamp*));
(* Retrieves the modification time of the data accessed by the given channel.
   If no such information is avaiblable, `ch.res' is set to `noModTime', 
   otherwise to `done'.  *)
  BEGIN
	mtime := 0;
	T.res :=Ch.noModTime;
	T.AssertValid();
  END GetModTime;

PROCEDURE (t: Text) NewReader*(): Reader, EXTENSIBLE; 
(* Attaches a new reader to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new reader is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Reader *)
BEGIN 
	RETURN NIL;
END NewReader;
  
PROCEDURE (t: Text) NewWriter*(): Writer, EXTENSIBLE;
(* Attaches a new writer to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new writer is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Writer *)
BEGIN 
	RETURN NIL;
END NewWriter;

PROCEDURE (f: Text) Close*;
BEGIN END Close;
(* Flushes all buffers associated with `f', closes the file, and frees all
   system resources allocated to it.  This invalidates all riders attached to
   `f', they can't be used further.  On success, i.e. if all read and write 
   operations (including flush) completed successfully, `f.res' is set to 
   `done'.  An opened file can only be closed once, successive calls of 
   `Close' are undefined.  
   Note that unlike the Oberon System all opened files have to be closed
   explicitly.  Otherwise resources allocated to them will remain blocked.  *)
  
(*not needed? : PROCEDURE (f: Text) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why the
   previous operation (NewReader, NewWriter, Flush, Close, etc.) failed.  The 
   string starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `r.res = done', then `descr' is assigned the empty string.  *)
*)

(*===========================================================*)
PROCEDURE (r: Reader) Pos*(): LONGINT;
(* Returns the current reading position associated with the reader `r' in
   channel `r.base', i.e. the index of the first byte that is read by the
   next call to ReadByte resp. ReadBytes.  This procedure will return 
   `noPosition' if the reader has no concept of a reading position (e.g. if it
   corresponds to input from keyboard), otherwise the result is not negative.*)
  BEGIN
	IF r.base.open THEN
		r.AssertValid();
		RETURN r.offset
	ELSE
		RETURN 0;
	END
  END Pos;

PROCEDURE (r: Reader) Available*(): LONGINT;
(* Returns the number of bytes available for the next reading operation.  For
   a file this is the length of the channel `r.base' minus the current reading
   position, for an sequential channel (or a channel designed to handle slow
   transfer rates) this is the number of bytes that can be accessed without
   additional waiting.  The result is -1 if Close() was called for the channel,
   or no more byte are available and the remote end of the channel has been
   closed.
   Note that the number of bytes returned is always a lower approximation of
   the number that could be read at once; for some channels or systems it might
   be as low as 1 even if tons of bytes are waiting to be processed.  *)
 	VAR
	i: LONGINT;
  BEGIN
    IF r. base. open THEN
      i := r. base. Length() - r. Pos();
	r.AssertValid();
      IF (i < 0) THEN
        RETURN 0
      ELSE
        RETURN i
      END
    ELSE
      RETURN -1
    END
  END Available;

(* not needed?: PROCEDURE (r: Reader) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why one of
   the previous operations (SetPos, ReadByte, or ReadBytes) failed.  The string
   starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `r.res = done', then `descr' is assigned the empty string.  *)
*)
(*===========================================================*)
(* type-bound procedures from Channel.Writer:
   Pos, SetPos, WriteByte, WriteBytes, ErrorDescr, ClearError *)

PROCEDURE (w: Writer) Pos*(): LONGINT;
(* Returns the current writing position associated with the writer `w' in
   channel `w.base', i.e. the index of the first byte that is written by the
   next call to WriteByte resp. WriteBytes.  This procedure will return 
   `noPosition' if the writer has no concept of a writing position (e.g. if it
   corresponds to output to terminal), otherwise the result is not negative. *)
  BEGIN
	IF w.base.open THEN
		RETURN w.offset
	ELSE
		RETURN 0;
	END
  END Pos;
  
PROCEDURE (w: Writer) SetInsertMode* (newMode: BOOLEAN), NEW;
BEGIN
w.insertMode := newMode;
END SetInsertMode;


(* not needed?:PROCEDURE (w: Writer) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why one of
   the previous operations (SetPos, WriteByte, or WriteBytes) failed.  The
   string starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `w.res = done', then `descr' is assigned the empty string.  *)
*)  

PROCEDURE (w: Writer) Truncate* (newLength: LONGINT), NEW, EXTENSIBLE;
(* Causes the file associated with `w' to have the specified length.  If the 
   file was previously larger than `newLength', the extra data is lost.  If it
   was previously shorter, bytes between the old and new lengths are read as 
   zeros.  The writer's position is not modified.
   Note: On systems that do not support shortening files directly it is 
   implemented as a partial file copy.  *)
BEGIN END Truncate;

(*****************************************************************************************)
(*=========== FullText Methods ==================================*)
(*****************************************************************************************)
PROCEDURE (t: FullText) Length*(): LONGINT, EXTENSIBLE;
(* Result is the number of bytes of data that this channel refers to.  If `ch'
   represents a file, then this value is the file's size.  If `ch' has no fixed
   length (e.g. because it's interactive), the result is `noLength'.  *)
  BEGIN
	RETURN t.txtmdl.Length();
  END Length;
  

PROCEDURE (t: FullText) NewReader*(): Reader, EXTENSIBLE; 
(* Attaches a new reader to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new reader is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Reader *)
VAR
	r: FullReader;
BEGIN 
	NEW(r);
	r.txtrdr := t.txtmdl.NewReader(NIL);
	r.offset := 0;
	r.txtrdr.SetPos(0);

	(* update inherited fields *)	
	r.base := t;
	r.res :=Ch.done;
	r.bytesRead := 0;
	r.positionable :=TRUE;
	
	t.res := Ch.done;
	r.AssertValid();
	RETURN r;
END NewReader;
  
PROCEDURE (t: FullText) NewWriter*(): Writer, EXTENSIBLE;
(* Attaches a new writer to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new writer is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Writer *)
VAR w: FullWriter;
BEGIN 
	NEW(w);
	w.txtwriter := t.txtmdl.NewWriter(NIL);
	w.offset := 0;
	w.insertMode := TRUE;
	w.txtwriter.SetPos(0);

	(* update inherited fields *)	
	w.base := t;
	w.res := Ch.done;
	w.bytesWritten := 0;
	w.positionable := TRUE; 
	
	t.res := Ch.done; 
	w.AssertValid();
	RETURN w;
END NewWriter;

(*===========================================================*)
(* type-bound procedures from Channel.Reader *)
PROCEDURE (r: FullReader) SetPos* (newPos: LONGINT), EXTENSIBLE;
(* Sets the reading position to `newPos'.  A negative value of `newPos' or 
   calling this procedure for a reader that doesn't allow positioning will set
   `r.res' to `outOfRange'.  A value larger than the channel's length is legal,
   but the following read operation will most likely fail with an 
   `readAfterEnd' error unless the channel has grown beyond this position in 
   the meantime.
   newPos must fit in INTEGER!!!
   Calls to this procedure while `r.res # done' will be ignored, in particular
   a call with `r.res = readAfterEnd' error will not reset `res' to `done'. *)
	VAR len : INTEGER; (* phys len of txt model *)
  BEGIN
    IF (r. res = Ch.done) THEN
      IF ~r. positionable OR (newPos < 0) THEN
        r. res :=  (Ch.outOfRange)
      ELSIF r. base. open THEN
	 	r.offset := SHORT(newPos);  (* offset can have any range >= 0 *)
	
		(* underlying txtmdl pos must lie within its length *)
	 	len := r.base(Text).txtmdl.Length();
	 	IF r.offset > len THEN
			r.txtrdr.SetPos(len);
		ELSE
			r.txtrdr.SetPos(r.offset);
		 END;
      ELSE  (* channel has been closed *)
        r. res :=  (Ch.channelClosed)
      END
    END;
	r.AssertValid();
  END SetPos;

PROCEDURE (r: FullReader) ReadByte* (VAR x: CHAR), EXTENSIBLE;
(* Reads a single byte from the channel `r.base' at the reading position 
   associated with `r' and places it in `x'.  The reading position is moved 
   forward by one byte on success, otherwise `r.res' is changed to indicate 
   the error cause.  Calling this procedure with the reader `r' placed at the 
   end (or beyond the end) of the channel will set `r.res' to `readAfterEnd'.
   `r.bytesRead' will be 1 on success and 0 on failure.
   Calls to this procedure while `r.res # done' will be ignored.  *)
(* Known Bug: If sel text truncated between calls to ReadByte or ReadBytes then
they will read a 0 byte before noticing the end of text.  Fix is to call r.txtrdr.text.Length()
 each time since r.txtrdr.eot is not reliable.*)
  BEGIN
    IF (r. res = Ch.done) THEN
      IF r. base. open THEN
		IF (r.Available() > 0) & (~r.txtrdr.eot) THEN 
			r.txtrdr.ReadChar(x); 
			r.bytesRead := (1);  INC(r.offset);
		ELSE
			x := 0X;
			r.bytesRead:=(0);
			r.res := (Ch.readAfterEnd);
		END
      ELSE  (* channel has been closed *)
        r. res :=  (Ch.channelClosed);
        r. bytesRead :=(0)
      END
    ELSE
      r. bytesRead:= (0)
    END;
	r.AssertValid();
  END ReadByte;

PROCEDURE (r: FullReader) ReadBytes* (VAR x: ARRAY OF CHAR; 
                                  start, n: LONGINT), EXTENSIBLE;
(* Reads `n' bytes from the channel `r.base' at the reading position associated
   with `r' and places them in `x', starting at index `start'.  The 
   reading position is moved forward by `n' bytes on success, otherwise 
   `r.res' is changed to indicate the error cause.  Calling this procedure with
   the reader `r' placed less than `n' bytes before the end of the channel will
   will set `r.res' to `readAfterEnd'.  `r.bytesRead' will hold the number of
   bytes that were actually read (being equal to `n' on success).
   Calls to this procedure while `r.res # done' will be ignored.
NOTE. impl limits n to MAX(INTEGER)
   pre: (n >= 0) & (0 <= start) & (start+n <= LEN (x)) *)
	VAR avail, i: LONGINT; 
  BEGIN
    ASSERT ((n >= 0) & (0 <= start) & (start+n <= LEN (x)));
    IF (r. res = Ch.done) THEN
      IF r. base. open THEN
		avail := r.Available();
		IF avail < n THEN n := avail END;
		i := 0;
(*		Log.String("zxxxx"); Log.Int(r.txtrdr.Base().Length()); Log.Ln;*)
		LOOP
			IF (i = n) OR r.txtrdr.eot THEN EXIT END;
			r.txtrdr.ReadChar(x[SHORT(start+i)]); 
			INC(i);
		END;
(*		Log.Int(SHORT(i));*)
		IF i < n THEN n:= i; END;
		IF n > 0 THEN 
			r.bytesRead:=(n);  INC(r.offset, n);
		ELSE
			(* removed this in Files. so here too? x[0] := 0X; *)
			r.bytesRead := (0);
			r.res := (Ch.readAfterEnd);
		END 
      ELSE  (* channel has been closed *)
        r. res :=  (Ch.channelClosed);
        r. bytesRead :=(0)
      END
    ELSE
      r. bytesRead :=(0)
	END;
	r.AssertValid();
  END ReadBytes;


(*===========================================================*)
(* type-bound procedures from Channel.Writer *)
PROCEDURE (w: FullWriter) SetPos* (newPos: LONGINT), EXTENSIBLE;
(* Sets the writing position to `newPos'.  A negative value of `newPos' or 
   calling this procedure for a writer that doesn't allow positioning will set
   `w.res' to `outOfRange'.  A value larger than the channel's length is legal,
   the following write operation will fill the gap between the end of the 
   channel and this position with zero bytes.
   newPos must fit in INTEGER!!!
   Calls to this procedure while `w.res # done' will be ignored.  *)
	VAR len : INTEGER; (* phys len of txt model *)
  BEGIN
    IF (w. res = Ch.done) THEN
      IF ~w. positionable OR (newPos < 0) THEN
        w. res :=  (Ch.outOfRange)
      ELSIF w. base. open THEN
	 	w.offset := SHORT(newPos);
		(* underlying txtmdl pos must lie within its length *)
	 	len := w.base(Text).txtmdl.Length();
	 	IF w.offset > len THEN
			w.txtwriter.SetPos(len);
		ELSE
			w.txtwriter.SetPos(w.offset);
		 END;
      ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed)
      END
    END
  END SetPos;

PROCEDURE (w: FullWriter) Pad (pad: CHAR; n: INTEGER),NEW, EXTENSIBLE;
(* Appends text with n chars of padChar.  Leaves w's offset and
 underlying model's offset at oldlen + n. *)
	VAR len : INTEGER; (* phys len of txt model *)
		i: INTEGER;
  BEGIN
 	len := w.base(Text).txtmdl.Length();
	w.txtwriter.SetPos(len);
	FOR i := 0 TO n-1 DO
		w.txtwriter.WriteChar(pad);
	END;
END Pad;

  
PROCEDURE (w: FullWriter) WriteByte* (x: CHAR), EXTENSIBLE;
(* Writes a single byte `x' to the channel `w.base' at the writing position 
   associated with `w'.  The writing position is moved forward by one byte on 
   success, otherwise `w.res' is changed to indicate the error cause.
   `w.bytesWritten' will be 1 on success and 0 on failure.
   Calls to this procedure while `w.res # done' will be ignored.  *)
	VAR len : INTEGER; (* phys len of txt model *)
  BEGIN
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(Text).txtmdl.Length();
(*		Log.String("writebyte ");Log.Int(SHORT(w.offset));Log.String(", "); Log.Int(len);Log.Ln;*)
		
		IF (w.offset > len) THEN
(*			Log.String("zero fill!! "); Log.Int(w.offset - len); Log.Ln; (* and set len = w.offset *)*)
			w.Pad(padChar, w.offset - len);
			len := w.offset;
		END;		
		IF (~w.insertMode) & (w.offset # len) THEN  (* if over top existing byte *)
			w.base(Text).txtmdl.Delete(w.offset, w.offset + 1); (* delete char *) 
(*			Log.String("overwrite "); Log.Ln;*)
			END;
		w.txtwriter.WriteChar(x);  
		INC(w.offset);
          w. bytesWritten:= (1);
      ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed);
        w. bytesWritten := (0)
      END
    ELSE
      w. bytesWritten := (0)
    END
  END WriteByte;

PROCEDURE (w: FullWriter) WriteBytes* (VAR x: ARRAY OF CHAR; start, n: LONGINT), EXTENSIBLE;
(* Writes `n' bytes from `x', starting at position `start', to the channel 
   `w.base' at the writing position associated with `w'.  The writing position
   is moved forward by `n' bytes on success, otherwise `w.res' is changed to 
   indicate the error cause.  `w.bytesWritten' will hold the number of bytes 
   that were actually written (being equal to `n' on success).
   Calls to this procedure while `w.res # done' will be ignored.
   pre: (n >= 0) & (0 <= start) & (start+n <= LEN (x))  *)
	VAR len : INTEGER; (* phys len of txt model *)
		i: LONGINT; numToDelete: INTEGER;
  BEGIN                                  
    ASSERT ((n >= 0) & (0 <= start) & (start+n <= LEN (x)));
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(Text).txtmdl.Length();
(*		Log.String("writebytes: ");Log.Int(SHORT(w.offset));Log.String(", "); Log.Int(len);Log.Ln;*)
		
		IF (w.offset > len) THEN
(*			Log.String("Zero fill!! "); Log.Int(w.offset - len); Log.Ln; (* and set len = w.offset *)*)
			w.Pad(padChar, w.offset - len);
			len := w.offset;
		END;		
		IF (~w.insertMode)  & (w.offset # len) THEN 
			IF SHORT(n) <= len THEN numToDelete := SHORT(n) ELSE numToDelete := len END;
			w.base(Text).txtmdl.Delete(w.offset, w.offset + numToDelete ); (* delete char *) 
(*			Log.String("overwrite "); Log.Int(numToDelete); Log.Ln;*)
		END;
		i := 0;
		LOOP
			IF (i = n) THEN EXIT END;
			w.txtwriter.WriteChar(x[SHORT(start+i)]); 
			INC(i);
		END;
(*		Log.Int(SHORT(i));*)
		w.bytesWritten := (n);  INC(w.offset, n);
      ELSE  (* channel has been closed *)
        w. res :=  (Ch.channelClosed);
        w. bytesWritten :=  (0)
      END
    ELSE
      w. bytesWritten :=  (0)
    END
  END WriteBytes;

PROCEDURE (w: FullWriter) Truncate* (newLength: LONGINT), EXTENSIBLE;
(* Causes the file associated with `w' to have the specified length.  If the 
   file was previously larger than `newLength', the extra data is lost.  If it
   was previously shorter, bytes between the old and new lengths are read as 
   zeros.  The writer's position is not modified.
   Note: On systems that do not support shortening files directly it is 
   implemented as a partial file copy.  *)
VAR len: INTEGER; w2: Writer;
BEGIN 
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(Text).txtmdl.Length();
(*		Log.String("trunc from "); Log.Int(len); Log.String(" to "); Log.Int(SHORT(newLength)); Log.Ln;*)
		IF (newLength < len) THEN
			w.base(Text).txtmdl.Delete(SHORT(newLength), len);
		ELSE (* pad file. use 2nd writer so our writer undisturbed *)
			w2 := w.base(Text).NewWriter();
			w2(FullWriter).Pad(padChar, SHORT(newLength) - len);
		END;
	 	len := w.base(Text).txtmdl.Length();
(*		Log.String("trunc now "); Log.Int(len); Log.Ln;*)
	 END
	END
END Truncate;

(*****************************************************************************************)
(*=========== SubText Methods ==================================*)
(*****************************************************************************************)
PROCEDURE (t: SubText) Length*(): LONGINT;
(* Result is the number of bytes of data that this channel refers to.  If `ch'
   represents a file, then this value is the file's size.  If `ch' has no fixed
   length (e.g. because it's interactive), the result is `noLength'.  *)
  BEGIN
	RETURN t.end - t.beg;
  END Length;
  

PROCEDURE (t: SubText) NewReader*(): Reader; 
(* Attaches a new reader to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new reader is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Reader *)
VAR
	r: SubReader;
BEGIN 
	NEW(r);
	r.txtrdr := t.txtmdl.NewReader(NIL);
	r.offset := 0;
	r.txtrdr.SetPos(t.beg + r.offset);

	(* update inherited fields *)	
	r.base :=(t);
	r.res := (Ch.done);
	r.bytesRead := (0);
	r.positionable:=(TRUE);
	
	t.res := (Ch.done);
	r.AssertValid();
	RETURN r;
END NewReader;
  
PROCEDURE (t: SubText) NewWriter*(): Writer;
(* Attaches a new writer to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new writer is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Writer *)
BEGIN 
	RETURN NIL; (* no writers for subtexts *)
END NewWriter;

PROCEDURE (r: SubReader) AssertValid();  (* private *)
(* check for invariant *)
BEGIN
r.AssertValid^();  (* use Oberon super-call operator *)
ASSERT(r.base(SubText).beg + r.offset <= r.base(SubText).end);
END AssertValid;


(*===========================================================*)
(* type-bound procedures from Channel.Reader *)
PROCEDURE (r: SubReader) SetPos* (newPos: LONGINT);
(* Sets the reading position to `newPos'.  A negative value of `newPos' or 
   calling this procedure for a reader that doesn't allow positioning will set
   `r.res' to `outOfRange'.  A value larger than the channel's length is legal,
   but the following read operation will most likely fail with an 
   `readAfterEnd' error unless the channel has grown beyond this position in 
   the meantime.
   newPos must fit in INTEGER!!!
   Calls to this procedure while `r.res # done' will be ignored, in particular
   a call with `r.res = readAfterEnd' error will not reset `res' to `done'. *)
	VAR len,beg : INTEGER; (* phys len of txt model *)
  BEGIN
    IF (r. res = Ch.done) THEN
      IF ~r. positionable OR (newPos < 0) OR (newPos > (r.base(SubText).end - r.base(SubText).beg)) THEN
        r. res :=  (Ch.outOfRange)
      ELSIF r. base. open THEN
	 	r.offset := SHORT(newPos);  (* offset can have any range >= 0 *)
	
		(* underlying txtmdl pos must lie within its length *)
	 	len := r.base(Text).txtmdl.Length();
	 	beg := r.base(SubText).beg;
	 	IF (beg + r.offset) > len THEN
			r.txtrdr.SetPos(len);
		ELSE
			r.txtrdr.SetPos(beg + r.offset);
		 END;
      ELSE  (* channel has been closed *)
        r. res :=  (Ch.channelClosed)
      END
    END;
	r.AssertValid();
  END SetPos;

(*  ReadByte, ReadBytes inherited *)


  
(*****************************************************************************************)
(*============== Module Fns ==================================*)
(*****************************************************************************************)
(* specialized file access; these procedures allow finer grained control than 
   the standard type-bound procedures Filename.New/Old *)

(*add other fns instead of New, Old, Tmp, Get/SetModTime, Exists *)
PROCEDURE New* (VAR res: INTEGER): Text;
(* Creates new text.  Use TextImpl.FullText
   `res' is set to `done'.  Otherwise result is NIL and `res' will indicate the
   problem.  *)
VAR
	T: FullText;
BEGIN 
    NEW(T);
	T.txtmdl :=  TextModels.dir.New();	(* allocate new empty text model *)
	res := Ch.done;
	
	(* set channel fields *)
	T.res :=  (Ch.done);
	T.readable :=(TRUE);
	T.writable :=(TRUE);
	T.open := (TRUE);
	RETURN T
END New;
  
PROCEDURE FocusText* (VAR res: INTEGER): Text;
(* Opens text with focus if any.
   `res' is set to `done'.  Otherwise result is NIL and `res' will indicate the
   problem.  *)
VAR
	T: FullText;
	C: TextControllers.Controller;
BEGIN 
   C := TextControllers.Focus();
   IF (C # NIL) THEN
	    NEW(T);
		T.txtmdl := C.text;
		res := Ch.done;
	
		(* set channel fields *)
		T.res :=  (Ch.done);
		T.readable :=(TRUE);
		T.writable :=(TRUE);
		T.open := (TRUE);
	ELSE
		T := NIL; 
		res := noFocusText; 
	END; 
	RETURN T
END FocusText;
  
PROCEDURE FocusSelection* (VAR res: INTEGER): Text;
(* Opens text with focus if any.
   `res' is set to `done'.  Otherwise result is NIL and `res' will indicate the
   problem.  *)
VAR
	T: SubText;
	C: TextControllers.Controller;
BEGIN 
   C := TextControllers.Focus();
   IF (C # NIL) & C.HasSelection() THEN
	    NEW(T);
		T.txtmdl := C.text;
		C.GetSelection(T.beg, T.end);
		res := Ch.done;

		(* set channel fields *)
		T.res :=  (Ch.done);
		T.readable :=(TRUE);
		T.writable:=(FALSE);  (* !not writable! *)
		T.open :=(TRUE);
	ELSE
		T := NIL; 
		IF C = NIL THEN res := noFocusText ELSE res := noFocusSelection END
	END; 
	RETURN T
END FocusSelection;
  
PROCEDURE ThisText* (bboxText: TextModels.Model; VAR res: INTEGER): Text;
(* Connect to the given Oberon environment specific text object.
   `res' is set to `done'.  Otherwise result is NIL and `res' will indicate the
   problem.  *)
VAR
	T: FullText;
BEGIN 
   IF (bboxText # NIL) THEN
	    NEW(T);
		T.txtmdl := bboxText;
		res := Ch.done;
	
		(* set channel fields *)
		T.res :=  (Ch.done);
		T.readable :=(TRUE);
		T.writable :=(TRUE);
		T.open:= (TRUE);
	ELSE
		T := NIL; 
		res := noText; 
	END; 
	RETURN T
END ThisText;
  
   
END OttTexts.
