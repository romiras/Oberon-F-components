MODULE ArcEnumDocs;

(* Enumerates all documents in all BlackBox subdirectories *)

	IMPORT Files, Strings, Out;
	
	TYPE
		String = Files.Name;
	
	VAR
		subsystem: String;
	
	PROCEDURE ProcessItem (name: String);
	BEGIN
		Out.String(name); Out.Ln;
	END ProcessItem;
	
	PROCEDURE ContainsDocuments (name: String): BOOLEAN;
	BEGIN
		(* according to BlackBox's documentation convention *)
		RETURN (name = "Docu") OR (name = "Spec")
			(*(name # "Code")
			& (name # "Mod") & (name # "Sym") & (name # "Rsrc")*)
	END ContainsDocuments;
	
	PROCEDURE WalkSubsystems (loc: Files.Locator; walkInto: BOOLEAN; rung: INTEGER);
		VAR li: Files.LocInfo; fi: Files.FileInfo; fname: String;
		
		PROCEDURE DocName (IN subsystem: ARRAY OF CHAR; VAR name: ARRAY OF CHAR);
		BEGIN
			IF subsystem # "System" THEN
				name := subsystem$ + name$
			END
		END DocName;
		
	BEGIN
		IF walkInto THEN
			fi := Files.dir.FileList(loc);
			WHILE fi # NIL DO
				fname := fi.name;
				DocName(subsystem, fname);
				ProcessItem(fname$);
				fi := fi.next
			END
		END;
		
		li := Files.dir.LocList(loc);
		WHILE li # NIL DO
			IF rung = 0 THEN subsystem := li.name END;
			IF (rung = 0) OR ((rung = 1) & ContainsDocuments (li.name)) THEN
				IF loc.res = 0 THEN WalkSubsystems(loc.This(li.name$), TRUE, rung + 1) END
			END;
			li := li.next
		END;
	END WalkSubsystems;
	
	PROCEDURE Do*;
		VAR loc: Files.Locator; li: Files.LocInfo;
	BEGIN
		loc := Files.dir.This(""); (* BlackBox's root directory *)
		WalkSubsystems (loc, FALSE, 0)
	END Do;

END ArcEnumDocs.Do