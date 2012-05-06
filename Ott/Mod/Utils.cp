MODULE OttUtils;
(****TLIB keywords*** "%n %v %f" *)
(* "CHANNEL.MOD 1.2 22-Jul-98,04:55:52" *)
(*   Useful functions for Ott Channels
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
   Ch := OttChannel, Log := StdLog, OttTextRider, 
	OttTexts, Views, TextViews, TextModels, TextControllers,  OttStrings, Files,
	OttAscii, OttLog;
  
(* CONST here *)
CONST
	done = 0;
	badParam = 1;
	notSupported = 2; (* can't do some operations on all Channel types *)
	
	(* max line length (not including 0X) for Copy and Compare *)
	maxLineLen* = 512;

(* VAR here *)
VAR
	res-: INTEGER;	(* used by all fns here, saves client having to declare a res var *)

(*===========================================================*)
PROCEDURE LoadIntoView*(chan: Ch.Channel);
(* connects channel to a view *)
	VAR v: Views.View;
BEGIN
	IF (chan # NIL) THEN
		WITH chan: OttTexts.Text DO
			v := TextViews.dir.New(chan.txtmdl);
			Views.OpenView(v);
			res := done;
		ELSE
			Log.String("loadintoview: non text channel type"); Log.Ln;
			res := badParam; (*!!later support File channels!!*)
		END
	ELSE
		res := badParam; (* NIL *)
	END;
END LoadIntoView;
	
(*===========================================================*)
PROCEDURE ToString*(chan: Ch.Channel; VAR buf: ARRAY OF CHAR);
(* copies conents of chan to buf *)
	VAR r: Ch.Reader; x: CHAR; i, n: INTEGER;
BEGIN
	IF (chan # NIL) THEN
		r := chan.NewReader();
		n := SHORT(r.Available());
		(*!!check n <= LEN(buf) *)
		FOR i := 0 TO n-1 DO
	(*		Log.Int(i); Log.String(" rd"); Log.Ln;*)
			r.ReadByte(x);
			buf[i] := x;	
		END;
		buf[i] := 0X;
(*		Log.Int(i); Log.String(" finalrd '"); Log.String(buf); Log.String("'");Log.Ln;*)
		res := done;
	ELSE
		res := badParam; (* NIL *)
	END;
END ToString;


(*===========================================================*)
PROCEDURE Clear*(chan: Ch.Channel);
(* Delete contents of chan *)
	VAR w: Ch.Writer; n: INTEGER;
BEGIN
	IF (chan # NIL) THEN
		w := chan.NewWriter(); w.SetPos(0);
		WITH w: OttTexts.Writer DO
			w.Truncate(0);  (* !!later optimize by reusing same w in OttUtils NewWriter(the_w) *)
			res := done;
(*!!		ELSE WITH w: OttFiles.Writer DO *)
		ELSE
			res := notSupported; (* !!need generic (and probably slower) way since Truncate not defined in Channel *)
		END;
	ELSE
		res := badParam; (* NIL *)
	END;
END Clear;

(*===========================================================*)
PROCEDURE FromString*(buf: ARRAY OF CHAR; chan: Ch.Channel);
(* Replace current contents of chan with contents of buf *)
(* see note in Copy *)
	VAR w: Ch.Writer; n: INTEGER;
BEGIN
	IF (chan # NIL) THEN
		IF res = done THEN
			w := chan.NewWriter(); w.SetPos(0);
			n := OttStrings.Length(buf); 
			w.WriteBytes(buf, 0, n);
(*			Log.String(" fromstr '"); Log.Int(n); Log.Ln;*)
			res := done;
		END
	ELSE
		res := badParam; (* NIL *)
	END;
END FromString;

(*===========================================================*)
PROCEDURE AppendFromString*(buf: ARRAY OF CHAR; chan: Ch.Channel);
(* Append current contents of chan with contents of buf *)
(* see note in Copy *)
	VAR w: Ch.Writer; n: INTEGER;
BEGIN
	IF (chan # NIL) THEN
		w := chan.NewWriter(); w.SetPos(chan.Length());
		n := OttStrings.Length(buf); 
		w.WriteBytes(buf, 0, n);
(*		Log.String(" appendfromstr '"); Log.Int(n); Log.Ln;*)
		res := done;
	ELSE
		res := badParam; (* NIL *)
	END;
END AppendFromString;

(*===========================================================*)
PROCEDURE Copy*(chanSrc, chanDst: Ch.Channel);
(* Line-by-line copy; eol of different channel types are handled. chanDst may
   be different length than chanSrc.
  Copies contents of chanSrc to chanDst.  chanDst not cleared first,
 so if its contents is non-empty then chanSrc may partially or completely
 replace its contents.  Decided not to call chanDst.Clear() here because
 then these util fns couldn't be used for a Log channel that is not truncatable. 
 Handles only lines up to maxLineLen bytes long. *)
	VAR r: OttTextRider.Reader; buf: ARRAY maxLineLen+1 OF CHAR; i: INTEGER;
		w: OttTextRider.Writer; savedPos: LONGINT;
BEGIN
	IF (chanSrc # NIL) & (chanDst # NIL) & chanSrc.open & chanDst.open 
		& chanSrc.readable & chanDst.writable THEN
		r := OttTextRider.ConnectReader(chanSrc); r.SetPos(0);
		IF ~r.byteReader.positionable THEN res := badParam; RETURN END;
		w := OttTextRider.ConnectWriter(chanDst);	w.SetPos(0);
		
		(* Set overwrite semantics and eol on the three channel types.
			Update this whenever add new channel type!! *)
		WITH chanDst: OttTexts.Text DO
			w.byteWriter(OttTexts.Writer).SetInsertMode(FALSE);
			buf[0] := OttAscii.cr; buf[1] := 0X;
			w.SetEol(buf, 1);
		| chanDst: OttLog.Log DO
			buf[0] := OttAscii.cr; buf[1] := 0X;
			w.SetEol(buf, 1);
		ELSE (* OttFiles already has overwrite and proper eol *)
		END;
Log.String("starting Copy"); Log.Ln;
		WHILE r.Res() = OttTextRider.done DO
			savedPos := r.Pos();
			r.ReadLine(buf); (* buf will be "" if empty line *)
			IF r.Res() # OttTextRider.done THEN (* err? re-read buf *)
				r.ClearError; r.SetPos(savedPos);
				i := -1;
				REPEAT
					INC(i);
					r.ReadChar(buf[i]); 
				UNTIL r.Res() # OttTextRider.done;
				buf[i] := 0X;
				IF i > 0 THEN
					Log.String("Reread. i "); Log.Int(i); Log.Ln;
					w.WriteString(buf); w.WriteLn;
				END
			ELSE
				w.WriteString(buf); w.WriteLn; 
			END;
		END;
		res := r.Res();
	ELSE
		res := badParam; (* NIL *)
	END;
END Copy;

(*!! do a line-by-line Compare fn!!*)

(*===========================================================*)
PROCEDURE RawCopy*(chanSrc, chanDst: Ch.Channel);
(* Byte-by-byte copy; no eol handling.
  copies contents of chanSrc to chanDst.  chanDst not cleared first,
 so if its contents is non-empty then chanSrc may partially or completely
 replace its contents.  Decided not to call chanDst.Clear() here because
 then these util fns couldn't be used for a Log channel that is not truncatable. *)
	VAR r: Ch.Reader; x: CHAR; i, n: INTEGER;
		w: Ch.Writer;
BEGIN
	IF (chanSrc # NIL) & (chanDst # NIL) & chanSrc.open & chanDst.open 
		& chanSrc.readable & chanDst.writable THEN
		r := chanSrc.NewReader(); r.SetPos(0);
		w := chanDst.NewWriter();	w.SetPos(0);
		WITH w: OttTexts.Writer DO
			w.SetInsertMode(FALSE);
		ELSE (* assume other types of channels have overwrite semantics *)
		END;
		n := SHORT(r.Available());
		FOR i := 0 TO n-1 DO
			r.ReadByte(x); w.WriteByte(x);
		END;
		res := done;
	ELSE
		res := badParam; (* NIL *)
	END;
END RawCopy;

(*===========================================================*)
PROCEDURE RawCompare*(chan1, chan2: Ch.Channel; res: INTEGER) : INTEGER;
(* Byte-by-byte compare; no eol handling.
  compares two channels. returns 0 if equal. -1 if chan1 shorter
 or a char is less chan chan2's.
 1 if chan2 longer or char > chan2. *)
	VAR r1: Ch.Reader; x1, x2: CHAR; i, n: INTEGER;
		r2: Ch.Reader;
BEGIN
	IF (chan1 # NIL) & (chan2 # NIL) & chan1.open & chan2.open 
		& chan1.readable & chan2.readable THEN
		r1 := chan1.NewReader(); r1.SetPos(0);
		r2 := chan2.NewReader(); r2.SetPos(0);
		n := SHORT(r1.Available());
		res := 0;
		IF ( n < SHORT(r2.Available())) THEN
			RETURN -1;
		ELSIF ( n > SHORT(r2.Available())) THEN
			RETURN 1;
		ELSE (* equal size -- check bytes *)
			FOR i := 0 TO n-1 DO
				r1.ReadByte(x1); r2.ReadByte(x2);
				IF (r1.res # Ch.done) OR (r2.res # Ch.done) THEN
					res := 99; (* ??? *)
					RETURN 0;
				END;
				IF (x1 < x2) THEN RETURN -1; 
				ELSIF (x1 > x2) THEN RETURN 1;
				END;
			END;
			RETURN 0; (* matches *)
		END
	ELSE
		res := badParam; (* NIL *)
		RETURN 0;
	END;
END RawCompare;

(*===========================================================*)
PROCEDURE DumpChannel*(chan: Ch.Channel);
(* Writes chan's fields to log.  Used in troubleshooting channels.
 *)
BEGIN
	Log.String("OttChannel res ");  Log.Int(chan.res); 
	Log.String(" readable ");  Log.Bool(chan.readable); 
	Log.String(" writable ");  Log.Bool(chan.writable); 
	Log.String(" open ");  Log.Bool(chan.open); 
	Log.Ln;
	Log.Ln;
END DumpChannel;

(*===========================================================*)
PROCEDURE DumpReader*(r: Ch.Reader);
(* Writes reader's fields to log.  Used in troubleshooting.
 *)
BEGIN
	Log.String("OttReader res ");  Log.Int(r.res); 
	Log.String(" bytesRead ");  Log.Int(SHORT(r.bytesRead));
	Log.String(" positionable ");  Log.Bool(r.positionable); 
	Log.Ln;
	Log.String(" available ");  Log.Int(SHORT(r.Available())); 
	Log.String(" pos ");  Log.Int(SHORT(r.Pos()));
	Log.Ln;
	DumpChannel(r.base);
END DumpReader;

END OttUtils.
(* end of file *)
