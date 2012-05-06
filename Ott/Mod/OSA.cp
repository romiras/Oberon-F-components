MODULE OttOSA; 
(****TLIB keywords*** "%n %v %f" *)
(* "CHARCL~1.MOD 1.2 22-Jul-98,05:01:32" *)
(*  Oberon System Abstraction Layer
	Low level module for non-portable functionality or
	for commonly used routines.
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
	Note. OTT is a modified and extended version of OOC.  This file
	however is unique to OTT.
	------------------------------------
*)

IMPORT
   Files, Views, TextModels, Converters, TextViews, OttStrings;
  
(* CONST here *)

(*===========================================================*)
PROCEDURE COPY*(s: ARRAY OF CHAR; OUT res: ARRAY OF CHAR);
(* replacement for COPY, which is not available in Bbox and res := s$ doesn't
	truncate 
Can't use VAR for s because then COPY("literal string", str) won't work.
Is there a better way that avoids copying s?!!
*)
BEGIN
(*broken: OttStrings.Extract(s, 0, OttStrings.Length(s) - 1, res);*)
res := s$;
END COPY;

(*===========================================================*)
PROCEDURE FilenameSplit*(file: ARRAY OF CHAR; VAR path, name: Files.Name);
(* Split the contents of file into path (eg "C:/dev/work/") and name (eg. "abc.txt")
*)
VAR i,n : INTEGER;
BEGIN
	n := OttStrings.Length(file);	i := n - 1;
	WHILE (i >= 0) & (file[i] # '/') DO 	(* walk file backwards looking for last / *)
		DEC(i);
	END;
	IF i < 0 THEN (* no / ? *)
		path := "";	name := file$;
	ELSE
		OttStrings.Extract(file, 0, i+1, path); (* up to and including the / *)
		OttStrings.Extract(file, i+1, 9999, name); (* everything after the / *)
	END
END FilenameSplit;

(*===========================================================*)
PROCEDURE GetFileExtension*(file: ARRAY OF CHAR; VAR name: Files.Name; VAR extension: Files.Type);
VAR i,n : INTEGER;
BEGIN
	n := OttStrings.Length(file);	i := n - 1;
	WHILE (i >= 0) & (file[i] # '.') DO 	(* walk file backwards looking for .*)
		DEC(i);
	END;
	IF i < 0 THEN (* no . ? *)
		extension := "";	name := file$;
	ELSE
		OttStrings.Extract(file, 0, i, name); (* up to and not including the . *)
		OttStrings.Extract(file, i+1, 9999, extension); (* everything after the . *)
	END;
END GetFileExtension;

(*===========================================================*)
PROCEDURE PutDoc*(textModel: TextModels.Model; dstPath: ARRAY OF CHAR; VAR res: INTEGER);
(* Stores the given native text object as a file with name dstPath.
	res = 0 success, 998 illegal path
*)
	VAR loc: Files.Locator; conv: Converters.Converter;
		v: Views.View; t: TextModels.Model; 
		path, name: Files.Name;
BEGIN
     FilenameSplit(dstPath, path, name);
	loc := Files.dir.This(path);
	IF loc = NIL THEN res := 998; RETURN; END;
	ASSERT(loc# NIL);
	t := textModel;
	v := TextViews.dir.New(t);	(* create a new text view for t *)
	conv := NIL;	(* no defaults for Views.Old *)
	Views.Register(v, Views.dontAsk, loc, name, conv, res)	;
END PutDoc;

(*===========================================================*)
PROCEDURE OpenDoc*(srcPath: ARRAY OF CHAR; VAR res: INTEGER): TextModels.Model;
(* Opens the given native text object as a textmodel with name srcPath.
	res = 0 success, 998 illegal path
*)
VAR loc: Files.Locator; path, name: Files.Name; conv: Converters.Converter;
		v: Views.View; t: TextModels.Model; 
BEGIN
     FilenameSplit(srcPath, path, name);
	loc := Files.dir.This(path);
	IF loc = NIL THEN res := 998; RETURN NIL; END;
	ASSERT(loc# NIL);
	conv := NIL;	(* no defaults for Views.Old *)
	v := Views.Old(Views.dontAsk, loc, name, conv);	(* don't ask user for a file and open it as a view *)
	IF (v # NIL) & (v IS TextViews.View) THEN		(* make sure it is a text view *)
		t := v(TextViews.View).ThisModel();	(* get the text view's model  *)
	ELSE
		res := 997;
		t := NIL;
	END;
	res := 0;
	RETURN t;	
END OpenDoc;
	
END OttOSA.


(* ----------------------------------


*)

(* end of file *)
