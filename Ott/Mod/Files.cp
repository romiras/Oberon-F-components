(*	$Id: Files.Mod,v 1.5 1997/06/20 14:11:42 acken Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
MODULE OttFiles ; (* [FOREIGN "C"; LINK FILE "Files.c" END]; <* Warnings := FALSE *> *)
  
(*  Access to files
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

(*!! ISSUES 
-file len may only be up to MAX SHORT since use SHORT in several places
*)

IMPORT
   Ch := OttChannel, Log := StdLog, Files, OttStrings, OttOSA;
  
(* CONST here *)
CONST  (* NOTE: refer to module Channel for the meaning of the various codes *)
	padChar = 0X; (* char to pad texts when write past eot.  *)

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

  (* these values report problems when opening or modifying a file: *)
  accessDenied* = Ch.freeErrorCode;
  (* access to the channel was denied, e.g. because a file's permissions don't
     permit the requested access method, or because the given URL isn't 
     publically readable *)
  isDirectory* = Ch.freeErrorCode+1;
  (* the `flags' argument specified write access, and the file is a 
    directory *)
  tooManyFiles* = Ch.freeErrorCode+2;
  (* the process or the entire system has too many files open *)
  noSuchFile* = Ch.freeErrorCode+3;
  (* the named file in a call to `Old' does not exist; or the directory part of
     a file name passed to `New' or `Tmp' does not exist *)
  directoryFull* = Ch.freeErrorCode+4;
  (* the directory or the file system that would contain the new file cannot be
     extended, either because there is no space left or the directory has a 
     fixed upper limit *)
  readOnlyFileSystem* = Ch.freeErrorCode+5;
  (* the file resides on a read-only file system and it is attempted to create
     a new file or to gain write access for an existing one  *)
  invalidTime* = Ch.freeErrorCode+6;
  (* the time passed to procedure SetModTime is no valid time stamp; either the
     millisecond part isn't valid, or the time value is too large or too small
     to be mapped to the time value of the underlying OS *)
  notOwner* = Ch.freeErrorCode+7;
  (* only the owner of a file can change its modification time *)
  anonymousFile* = Ch.freeErrorCode+8;
  (* a file can only be registered if a file name was passed to the initial 
    call to Tmp() *)
  dirWriteDenied* = Ch.freeErrorCode+9;
  (* you need to have write permission for the directory you want to add a new
     file to *)
  fileError* = Ch.freeErrorCode+10;
  (* unspecified error when opening/creating a file; this usually means that
     this module doesn't know how to interpret the error code delivered by 
     the OS *)
  nameTooLong* = Ch.freeErrorCode+11;
  (* either the total length of the file name or of an individual file name
     component is too large; the operating system can impose such limits (see
     PATH_MAX and NAME_MAX in /usr/include/limits.h), or the file system itself
     restricts the format of names on it *) 
  notDirectory* = Ch.freeErrorCode+12;
  (* a file that is referenced as a directory component of the file name 
     exists, but is not a directory *)
  linkLoop* = Ch.freeErrorCode+13;
  (* too many symbolic links were resolved while trying to look up the file
     name; the operating system has an arbitrary limit on the number of 
     symbolic links that may be resolved in looking up a single file name,
     as a primitive way to detect loops *)
  notImplemented* = Ch.freeErrorCode+14;
  (* Requested action not implemented in this version.  This usually
 	indicates a non-compliance with OOC lib "standard". *)
     
  
CONST
  (* possible elements for `flags' parameter of New/Old/Tmp: *)
  read* = 0;
  (* if the file cannot be opened for reading access, then it isn't opened at
     all; in this case the error code is set to `noReadAccess' *)
  write* = 1;
  (* if the file cannot be opened for writing access, then it isn't opened at
     all; in this case the error code is set to `noWriteAccess' *)
  tryRead* = 2;
  (* try to open this file for reading access; if the file permissions don't 
     permit reading the file is opened nevertheless, but the file descriptor's
     attribute `readable' is set to FALSE *)
  tryWrite* = 3;
  (* try to open this file for writing access; if the file permissions don't 
     permit writing the file is opened nevertheless, but the file descriptor's
     attribute `writable' is set to FALSE *)
  (* note: at least one of the above flags has to be set; otherwise you'll 
     always get an `access denied' error *)
(*--- concrete File ---*)
TYPE
	File* = POINTER TO FileDesc;
	FileDesc* = EXTENSIBLE RECORD (Ch.ChannelDesc)
		frep-: Files.File;
		isTmp: BOOLEAN;  	(* whether created by Tmp.  Reset by Register *)
		tmpName: Files.Name; (* used by Tmp for later call to Register.  *)
	END;

	Reader* =  POINTER TO ReaderDesc;
	ReaderDesc* = EXTENSIBLE RECORD (Ch.ReaderDesc)
		frdr:	Files.Reader;
		offset:    INTEGER;  
	END;

	Writer* = POINTER TO WriterDesc;
	WriterDesc* = EXTENSIBLE RECORD (Ch.WriterDesc)
		fwriter:	Files.Writer;
		offset:    INTEGER;  (* 0 .. (end - beg) *)
	END;

(*===========================================================*)
(* BYTE, CHAR conversion routines *)
PROCEDURE ByteToChar(byt: BYTE) : CHAR;
VAR x: CHAR;
BEGIN
	IF byt >= 0 THEN 
		x := CHR(byt)
	ELSE
		x := CHR(256 + byt);
	END;
	RETURN x;
END ByteToChar;

PROCEDURE CharToByte(x: CHAR) : BYTE;
VAR byt: BYTE;
BEGIN
	IF x < 80X THEN 
		byt := SHORT(SHORT(ORD(x)));
	ELSE
		byt := SHORT(SHORT( ORD(x) - 256 ));
	END;
	RETURN byt;
END CharToByte;

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
	| accessDenied:	str := "Access denied."
	| isDirectory:		str := "Can't get write access to directory file."
	| tooManyFiles:	str := "Too many open files at the moment."
	| noSuchFile:		str := "The named file does not exist."
	| directoryFull:	str := "Can't add new files to directory."
	| readOnlyFileSystem: str := "File system is read-only."
	| invalidTime:		str := "Invalid modification time."
	| notOwner:		str := "Not owner."
	| anonymousFile:	str := "Can't register an anonymous file."
	| dirWriteDenied:	str := "Don't have write permission for directory."
	| fileError:			str := "Failed to open the file."
	| nameTooLong:	str := "The file name or one of its components is too long."
	| notDirectory:		str := "A directory component of the file name exists, but isn't a directory"
	| linkLoop:		    str := "Resolved too many symbolic links while looking up the file"
	| notImplemented:   str := "Requested function not implemented."
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
	ASSERT(r.frdr # NIL, 100);
END AssertValid;

PROCEDURE (w: Writer) AssertValid(), NEW, EXTENSIBLE;  (* private *)
(* check for invariant *)
BEGIN
	ASSERT(w.offset >= 0, 100);
	(* can't check against underlying text model because it can be changed at
	any time by other clients *)
	ASSERT(w.fwriter # NIL, 100);
END AssertValid;

PROCEDURE (f: File) AssertValid(), NEW;  (* private *)
(* check for invariant *)
BEGIN
	(* can't check against underlying text model because it can be changed at
	any time by other clients *)
	ASSERT(f.frep # NIL, 100);
END AssertValid;

(*****************************************************************************************)
(*=========== File Methods ==================================*)
(*****************************************************************************************)
(*===========================================================*)
(* type-bound procedures from Channel.Channel:
   NewWriter, Flush, Close, ErrorDescr *)
PROCEDURE (f: File) Length*(): LONGINT, EXTENSIBLE;
(* Result is the number of bytes of data that this channel refers to.  If `ch'
   represents a file, then this value is the file's size.  If `ch' has no fixed
   length (e.g. because it's interactive), the result is `noLength'.  *)
BEGIN
	RETURN f.frep.Length()
END Length;
  
PROCEDURE (f: File) GetModTime* (VAR mtime: INTEGER (*Time.TimeStamp*));
(* Retrieves the modification time of the data accessed by the given channel.
   If no such information is avaiblable, `ch.res' is set to `noModTime', 
   otherwise to `done'.  *)
BEGIN
	mtime := 0; (* do later, bbox has no getmodtime!! *)
	f.res := (Ch.noModTime);
	f.AssertValid()
END GetModTime;


PROCEDURE (f: File) Register*, NEW, EXTENSIBLE;
(* Registers the file `f' in the directory structure if it has been created 
   with the `Tmp' procedure below.  Registration happens atomically, i.e., it
   is guaranteed that any previously existing file is replaced by the newly 
   registered one without any "in between" state.  If the operation is 
   interrupted, then either the old file still exists on the file system, or
   it has been replaced completely by the new one.
     Calling `Tmp' and `Register' successively  has the same effect as calling
   `New'.  Calling this procedure has no effect if the file `f' has been 
   created with `New' or has been registered previously.  Registration fails
   with an `anonymousFile' error if it was created by calling `Tmp' with an
   empty file name, and with a `channelClosed' error if `f' is closed.  
	BBox implementation: File is closed after Register.
*)
VAR
	name: Files.Name; extension: Files.Type;
	resForTheFile: INTEGER;
BEGIN
	IF ~f.open  THEN
		f.res := (channelClosed)
	ELSIF f.isTmp THEN
		IF OttStrings.Length(f.tmpName) = 0 THEN
			f.res := (anonymousFile)
		ELSE
			name := f.tmpName;
			OttOSA.GetFileExtension(name, name, extension);
			f.frep.Register(name, extension, Files.dontAsk, resForTheFile); 
			f.frep.Close;
			IF resForTheFile # 0 THEN
				CASE resForTheFile OF
					2:	f.res := (noSuchFile)
				|	4:	f.res := (dirWriteDenied)
				ELSE	f.res := (fileError)
				END;
			END;
			f.isTmp := FALSE; (* mark file as registered *)
			f.open := (FALSE);
		END
	END;
END Register;
  
PROCEDURE (f: File) NewReader*(): Reader, EXTENSIBLE; 
(* Attaches a new reader to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new reader is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Reader *)
VAR
	r: Reader;
BEGIN 
	IF ~f.readable THEN
		f.res := (Ch.noReadAccess);
		RETURN NIL;
	ELSE
		NEW(r);
		r.frdr := f.frep.NewReader(NIL);
		r.offset := 0;
		r.frdr.SetPos(0);

		(* update inherited fields *)	
		r.base:= (f);
		r.res := (Ch.done);
		r.bytesRead := (0);
		r.positionable:=(TRUE);
	
		f.res := (Ch.done);
		r.AssertValid();
		RETURN r;
	END
END NewReader;
  
PROCEDURE (f: File) NewWriter*(): Writer, EXTENSIBLE;
(* Attaches a new writer to the file `f'.  It is placed at the very start 
   of the file, and its `res' field is initialized to `done'.  `f.res' is
   set to `done' on success and the new writer is returned.  Otherwise result 
   is NIL and `f.res' is changed to indicate the error cause.  
fn exists to change signature to return OttTexts.Writer *)
VAR w: Writer;
BEGIN 
	IF ~f.writable THEN
		f.res := (Ch.noWriteAccess);
		RETURN NIL;
	ELSE
		NEW(w);
		w.fwriter := f.frep.NewWriter(NIL);
		w.offset := 0;
		w.fwriter.SetPos(0);

		(* update inherited fields *)	
		w.base:=(f);
		w.res := (Ch.done);
		w.bytesWritten := (0);
		w.positionable:=(TRUE);
	
		f.res := (Ch.done);
		w.AssertValid();
		RETURN w;
	END		
END NewWriter;

PROCEDURE (f: File) Flush*, EXTENSIBLE;
(* Flushes all buffers related to this channel.  Any pending write operations
   are passed to the underlying OS and all buffers are marked as invalid.  The
   next read operation will get its data directly from the channel instead of 
   the buffer.  If a writing error occurs during flushing, the field `ch.res'
   will be changed to `writeError', otherwise it's assigned `done'.  Note that
   you have to check the channel's `res' flag after an explicit flush yourself,
   since none of the attached writers will notice any write error in this 
   case.  
   BBox Files: flush has no error return;
*)
BEGIN
	f.frep.Flush();
	f. ClearError
END Flush;

PROCEDURE (f: File) Close*;
VAR res: INTEGER;
BEGIN 
	IF f # NIL THEN
		f.frep.Close;
		f.open := (FALSE);
	END
END Close;
(* Flushes all buffers associated with `f', closes the file, and frees all
   system resources allocated to it.  This invalidates all riders attached to
   `f', they can't be used further.  On success, i.e. if all read and write 
   operations (including flush) completed successfully, `f.res' is set to 
   `done'.  An opened file can only be closed once, successive calls of 
   `Close' are undefined.  
   Note that unlike the Oberon System all opened files have to be closed
   explicitly.  Otherwise resources allocated to them will remain blocked.  *)
  
PROCEDURE (f: File) ErrorDescr* (VAR descr: ARRAY OF CHAR);
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

PROCEDURE (r: Reader) SetPos* (newPos: LONGINT), EXTENSIBLE;
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
			len := r.base(File).frep.Length();
			IF r.offset > len THEN
				r.frdr.SetPos(len);
			ELSE
				r.frdr.SetPos(r.offset);
			END
		ELSE  (* channel has been closed *)
			r. res :=  (Ch.channelClosed)
		END
	END;
	r.AssertValid()
END SetPos;

PROCEDURE (r: Reader) ReadByte* (VAR x: CHAR), EXTENSIBLE;
(* Reads a single byte from the channel `r.base' at the reading position 
   associated with `r' and places it in `x'.  The reading position is moved 
   forward by one byte on success, otherwise `r.res' is changed to indicate 
   the error cause.  Calling this procedure with the reader `r' placed at the 
   end (or beyond the end) of the channel will set `r.res' to `readAfterEnd'.
   `r.bytesRead' will be 1 on success and 0 on failure.
   Calls to this procedure while `r.res # done' will be ignored.  *)
(* Known Bug: If sel text truncated between calls to ReadByte or ReadBytes then
they will read a 0 byte before noticing the end of text.  Fix is to call r.frdr.text.Length()
 each time since r.frdr.eof is not reliable.*)
VAR byt: BYTE;
BEGIN
	IF (r. res = Ch.done) THEN
		IF r. base. open THEN
			IF (r.Available() > 0) & (~r.frdr.eof) THEN 
				r.frdr.ReadByte(byt); 
				x := ByteToChar(byt);
				r.bytesRead := (1);  INC(r.offset);
			ELSE
				x := 0X;
				r.bytesRead := (0);
				r.res := (Ch.readAfterEnd);
			END
		ELSE  (* channel has been closed *)
			r. res :=  (Ch.channelClosed);
			r. bytesRead :=  (0)
		END
	ELSE
		r. bytesRead :=  (0)
	END;
	r.AssertValid();
END ReadByte;

PROCEDURE (r: Reader) ReadBytes* (VAR x: ARRAY OF CHAR; 
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
	VAR avail, i, numToRead: LONGINT; byt: BYTE;
BEGIN
	ASSERT ((n >= 0) & (0 <= start) & (start+n <= LEN (x)));
	IF (r. res = Ch.done) THEN
		IF r. base. open THEN
			avail := r.Available();
			numToRead := n;
			IF avail < n THEN numToRead := avail END;
			i := 0;
(*			Log.String("zxxxx"); Log.Int(r.frdr.Base().Length()); Log.Ln;*)
			LOOP
				IF (i = numToRead) OR r.frdr.eof THEN EXIT END;
				r.frdr.ReadByte(byt); 
				x[SHORT(start + i)] := ByteToChar(byt);
				INC(i);
			END;
	(*		Log.Int(SHORT(i));*)
			IF i > 0 THEN	(* read something? *)
				IF i = n THEN
					r.bytesRead := (n);  INC(r.offset, n);
				ELSE
					ASSERT(i < n);
					r.bytesRead := (i);  INC(r.offset, i);
					r.res := (Ch.readAfterEnd);
				END
			ELSE
				r.bytesRead := (0);
				r.res := (Ch.readAfterEnd);
			END 
		ELSE  (* channel has been closed *)
			r. res :=  (Ch.channelClosed);
			r. bytesRead :=  (0)
		END
	ELSE
		r. bytesRead :=  (0)
	END;
	r.AssertValid()
END ReadBytes;

PROCEDURE (r: Reader) ErrorDescr* (VAR descr: ARRAY OF CHAR);
(* Retrieves a descriptive error message string stating the reason why one of
   the previous operations (SetPos, ReadByte, or ReadBytes) failed.  The string
   starts with a capital letter and does not include any termination 
   punctuation.  `descr' should be large enough to hold a multi-line message 
   (256 characters should suffice).
   If `r.res = done', then `descr' is assigned the empty string.  *)
BEGIN
	ErrorDescr (r. res, descr)
END ErrorDescr;


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
  

PROCEDURE (w: Writer) SetPos* (newPos: LONGINT), EXTENSIBLE;
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
			(* underlying frep pos must lie within its length *)
			len := w.base(File).frep.Length();
			IF w.offset > len THEN
				w.fwriter.SetPos(len);
			ELSE
				w.fwriter.SetPos(w.offset);
			END;
		ELSE  (* channel has been closed *)
			w. res :=  (Ch.channelClosed)
		END
    END
  END SetPos;

PROCEDURE (w: Writer) Pad (pad: CHAR; n: INTEGER),NEW, EXTENSIBLE;
(* Appends text with n chars of padChar.  Leaves w's offset and
 underlying model's offset at oldlen + n. *)
	VAR len : INTEGER; (* phys len of txt model *)
		i: INTEGER; byt: BYTE;
  BEGIN
	len := w.base(File).frep.Length();
	w.fwriter.SetPos(len);
	byt := CharToByte(pad);
	FOR i := 0 TO n-1 DO
		w.fwriter.WriteByte(byt);
	END;
END Pad;

  
PROCEDURE (w: Writer) WriteByte* (x: CHAR), EXTENSIBLE;
(* Writes a single byte `x' to the channel `w.base' at the writing position 
   associated with `w'.  The writing position is moved forward by one byte on 
   success, otherwise `w.res' is changed to indicate the error cause.
   `w.bytesWritten' will be 1 on success and 0 on failure.
   Calls to this procedure while `w.res # done' will be ignored.  *)
	VAR len : INTEGER; (* phys len of txt model *)
		byt: BYTE;
  BEGIN
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(File).frep.Length();
(*		Log.String("writebyte ");Log.Int(SHORT(w.offset));Log.String(", "); Log.Int(len);Log.Ln;*)
		
		IF (w.offset > len) THEN
(*			Log.String("zero fill!! "); Log.Int(w.offset - len); Log.Ln; (* and set len = w.offset *)*)
			w.Pad(padChar, w.offset - len);
		END;		
		byt := CharToByte(x);
		w.fwriter.WriteByte(byt);
		INC(w.offset);
          w. bytesWritten :=  (1);
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
	VAR len : INTEGER; (* phys len of txt model *)
		i: LONGINT; byt: BYTE;
  BEGIN                                  
    ASSERT ((n >= 0) & (0 <= start) & (start+n <= LEN (x)));
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(File).frep.Length();
(*		Log.String("writebytes ");Log.Int(SHORT(w.offset));Log.String(", "); Log.Int(len);Log.Ln;*)
		
		IF (w.offset > len) THEN
(*			Log.String("zero fill!! "); Log.Int(w.offset - len); Log.Ln; (* and set len = w.offset *)*)
			w.Pad(padChar, w.offset - len);
		END;		
		i := 0;
		LOOP
			IF (i = n) THEN EXIT END;
			byt := CharToByte(x[SHORT(start+i)]);
			w.fwriter.WriteByte(byt); 
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

PROCEDURE (w: Writer) Truncate* (newLength: LONGINT), NEW, EXTENSIBLE;
(* Causes the file associated with `w' to have the specified length.  If the 
   file was previously larger than `newLength', the extra data is lost.  If it
   was previously shorter, bytes between the old and new lengths are read as 
   zeros.  The writer's position is not modified.
   Note: On systems that do not support shortening files directly it is 
   implemented as a partial file copy.  
!!NOTE: Current implementation only supports lengthening a file. 
*)
VAR len: INTEGER; w2: Writer;
BEGIN 
    IF (w. res = Ch.done) THEN
      IF w. base. open THEN
	 	len := w.base(File).frep.Length();
(*		Log.String("trunc from "); Log.Int(len); Log.String(" to "); Log.Int(SHORT(newLength)); Log.Ln;*)
		IF (newLength < len) THEN
			w.res := (notImplemented); (* implement partial file copy later!! *)
		ELSE (* pad file. use 2nd writer so our writer undisturbed *)
			w2 := w.base(File).NewWriter();
			w2.Pad(padChar, SHORT(newLength) - len);
		END;
	 	len := w.base(File).frep.Length();
(*		Log.String("trunc now "); Log.Int(len); Log.Ln;*)
	 END
	END
END Truncate;


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
(* specialized file access; these procedures allow finer grained control than 
   the standard type-bound procedures Filename.New/Old *)
   

  
PROCEDURE Old* (file: ARRAY OF CHAR; flags: SET; VAR res: INTEGER): File;
(* Opens an existing file.  On success the new file handle is returned and
   `res' is set to `done'.  Otherwise result is NIL and `res' will indicate the
   problem.  
Comments.  
-use / as path delimiter (for portability)
-Oberon has no notion of a current dir as in UNIX pwd.  So the following rules wil
 be applied to the variable file
-Old with write flags will fail if file already open, even if previous Old was in read.

 relative paths		directory
 "abc.txt"   			BBox home dir eg. C:/blackbox
 "mydir/abc.txt"   	relative path from BBox home dir eg C:/blackbox/mydir

 absolute paths
 "/abc.txt"			root of drive containing BBox home dir  eg C:/
 "/mydir/abc.txt"	absolute path of root of BBox home dir eg C:/mydir
 "d:abc.txt"		   current dir of given drive.  Dangerous!!
 "d:/mydir/abc.txt"  absolute path on given drive
 "//mylandir/mydir/abc.txt" Win95 connected lan drive
*)
VAR f: File;
	loc: Files.Locator;
	theFile : Files.File;
	shared : BOOLEAN;
	path, name : Files.Name;
BEGIN 
     IF ~((read IN flags) OR (write IN flags) OR (tryRead IN flags) OR (tryWrite IN flags)) THEN
		res := accessDenied ;
		RETURN NIL;
	END;
     OttOSA.FilenameSplit(file, path, name);
	loc := Files.dir.This(path);
	shared := ~((write IN flags) OR (tryWrite  IN flags));
	theFile := Files.dir.Old(loc, name, shared);
(*Log.String("Old res = "); Log.Int(loc.res); Log.String(" path = <"); Log.String(path); Log.String(">"); Log.Ln;*)
	IF (theFile =  NIL) THEN 
		IF (tryWrite IN flags) THEN
			RETURN Old(name, {read}, res); (* ! recursion ! *)
		END;
		CASE loc.res OF
			0,2:   res:= noSuchFile
		|	1:	res := fileError
		|	6:	res:= accessDenied
		ELSE	res := fileError
		END;
		RETURN NIL;
	ELSE
		res := Ch.done;
	
	    NEW(f);
		f.frep := theFile;
		f.tmpName := ""; f.isTmp := FALSE;
		(* set channel fields *)
		f.res :=  (Ch.done);
		f.readable:= ((read IN flags) OR (tryRead IN flags));
		f.writable :=( (write IN flags) OR (tryWrite IN flags));
		f.open :=  (TRUE);
		RETURN f
	END
END Old;
  
PROCEDURE New* (file: ARRAY OF CHAR; flags: SET; VAR res: INTEGER): File;
(* Creates a new file under the given name.  On success the new file handle is
   returned and `res' is set to `done'.  Otherwise result is NIL and `res'
   and will indicate the problem.
   Note that in terms of the Oberon System this procedure combines the 
   procedures New and Register.  *)
VAR f: File;
	loc: Files.Locator; path, name: Files.Name; extension: Files.Type;
	theFile : Files.File;  resForTheFile: INTEGER;
BEGIN 
     IF ~((read IN flags) OR (write IN flags) OR (tryRead IN flags) OR (tryWrite IN flags)) THEN
		res := accessDenied ;
		RETURN NIL;
	END;
     OttOSA.FilenameSplit(file, path, name);
	loc := Files.dir.This(path);
	ASSERT(loc# NIL);
	theFile := Files.dir.New(loc, Files.dontAsk);
	IF (theFile = NIL) THEN
		CASE loc.res OF
			1,2:   res:= noSuchFile
		|	4:	res:= dirWriteDenied
		ELSE	res := fileError
		END;
		RETURN NIL;
	END;
	OttOSA.GetFileExtension(name, name, extension);		
	theFile.Register(name, extension, Files.dontAsk, resForTheFile); 
	theFile.Close;
	IF resForTheFile # 0 THEN
		CASE resForTheFile OF
			2:	res := noSuchFile
		|	4:	res := dirWriteDenied
		ELSE	res := fileError
		END;
		RETURN NIL;
	END;
		
	f :=Old(file, flags, res);
	RETURN f
END New;

PROCEDURE Tmp* (file: ARRAY OF CHAR; flags: SET; VAR res: INTEGER): File;
(* Creates a temporary file that can be registered later on.  On success the 
   new file handle is returned and `res' is set to `done'.  Otherwise result
   is NIL and `res' will indicate the problem.
     Temporary files are created with an empty permission list, the 
   permissions are extended upon registration.  The files are deleted if they
   haven't been registered and are closed or the program terminates.  
     An unique temporary file name is created if the given file name is the 
   empty string.  Such a file can't be registered later.  Note that some 
   systems are said to have a very low limit for the number of temporary file 
   names.  The limit is never less than 25.  To be on the safe side you should
   never have more than 25 anonymous temporary files open simultaneously, or 
   check that the TMP_MAX macro in /usr/include/stdio.h is large enough for 
   your purposes.
     With oo2c if `file' isn't empty, the new name is derived from the old one
   by appending "^", "^1", "^2", etc. in turn, until a file name is found that
   doesn't exist already.  If such call to `Tmp' returns `nameTooLong', then 
   this refers to the constructed temporary name, not the one in `file'.  
     This function corresponds to Oberon System's New.  *)
VAR f: File;
 loc: Files.Locator; path, name: Files.Name;
 theFile : Files.File;
BEGIN 
	IF ~((read IN flags) OR (write IN flags) OR (tryRead IN flags) OR (tryWrite IN flags)) THEN
		res := accessDenied ;
		RETURN NIL;
	END;
	OttOSA.FilenameSplit(file, path, name);
	loc := Files.dir.This(path);
	ASSERT(loc# NIL);
	theFile := Files.dir.New(loc, Files.dontAsk);
	IF (theFile = NIL) THEN
		CASE loc.res OF
			1,2:   res:= noSuchFile
		|	4:	res:= dirWriteDenied
		ELSE	res := fileError
		END;
		RETURN NIL;
	ELSE
		res := Ch.done;
	
	    NEW(f);
		f.frep := theFile;
		OttOSA.COPY(name, f.tmpName); (* save for Register *)
		f.isTmp := TRUE;
		(* set channel fields *)
		f.res :=  (Ch.done);
		f.readable:= ((read IN flags) OR (tryRead IN flags));
		f.writable:=( (write IN flags) OR (tryWrite IN flags));
		f.open :=  (TRUE);
		RETURN f
	END
END Tmp;
  
PROCEDURE SetModTime* (file: ARRAY OF CHAR; mtime: LONGINT (*Time.TimeStamp*); VAR res: INTEGER);
(* Sets the modification time of the given file to `mtime'.  On success `res'
   will contain `done', otherwise an error code that'll indicate the problem.
   Note that under Unix this procedure will also change the access time to the
   value of `mtime'.  *)
BEGIN 
	mtime := 0; (* do later, bbox has no getmodtime!! *)
	res := noModTime;
END SetModTime;

PROCEDURE GetModTime* (file: ARRAY OF CHAR; VAR mtime: LONGINT (*Time.TimeStamp*); VAR res: INTEGER);
(* Gets the modification time of the given file to `mtime'.  On success `res'
   will contain `done', otherwise an error code indicating the problem.*)
BEGIN 
	mtime := 0; (* do later, bbox has no getmodtime!! *)
	res := noModTime;
END GetModTime;

PROCEDURE Exists* (file: ARRAY OF CHAR): BOOLEAN;
(* Returns TRUE if a file `file' exists, FALSE otherwise. 
   ... will be changed to give more useful information on failure *)
VAR
	loc: Files.Locator;
	theFile : Files.File;
	path, name : Files.Name;
BEGIN 
	OttOSA.FilenameSplit(file, path, name);
	loc := Files.dir.This(path);
	theFile := Files.dir.Old(loc, name, TRUE);
	RETURN (theFile # NIL); (* FALSE may indicate other error than file-not-exist! *)
END Exists;
  
END OttFiles.
