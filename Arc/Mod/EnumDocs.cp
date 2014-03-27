MODULE ArcEnumDocs;

(* Enumerates all documents in all BlackBox subdirectories *)

	IMPORT Files, Strings, Out;
	
	TYPE
		String* = Files.Name;
		
		DocList* = POINTER TO RECORD
			next-: DocList;
			loc-: Files.Locator;
			subName-,
			docName-: String
		END;
	
	VAR
		subsystem: String;
		listRoot-, listHead-: DocList;
	
	PROCEDURE AddDocument (loc: Files.Locator; IN sub, doc: ARRAY OF CHAR);
		VAR node: DocList;
	BEGIN
		NEW(node);
		node.loc := loc;
		node.subName := sub$;
		node.docName := doc$;
		IF listHead # NIL THEN listHead.next := node END;
		listHead := node;
		IF listRoot = NIL THEN listRoot := listHead END
	END AddDocument;
	
	PROCEDURE ContainsDocuments (name: String): BOOLEAN;
	BEGIN
		(* according to BlackBox's documentation convention *)
		RETURN (name = "Docu") OR (name = "Spec") OR (name = "Bab")
			(*(name # "Code")
			& (name # "Mod") & (name # "Sym") & (name # "Rsrc")*)
	END ContainsDocuments;
	
	PROCEDURE ScanSubsystems* (loc: Files.Locator; listEntries: BOOLEAN; rung: INTEGER);
		VAR li: Files.LocInfo; fi: Files.FileInfo; fname: String;
		
		PROCEDURE CleanName (VAR name: ARRAY OF CHAR): BOOLEAN;
			VAR pos: INTEGER;
		BEGIN
			Strings.Find(name, "odc", 0, pos);
			IF pos > 0 THEN
				Strings.Extract(name, 0, pos - 1, name);
				RETURN TRUE
			ELSE
				Out.String("-## Not ODC: " + name$); Out.Ln;
				RETURN FALSE
			END
		END CleanName;
		
	BEGIN
		IF listEntries THEN
			fi := Files.dir.FileList(loc);
			WHILE fi # NIL DO
				fname := fi.name;
				IF CleanName(fname) THEN
					AddDocument(loc, subsystem$, fname$)
				END;
				fi := fi.next
			END
		END;
		
		li := Files.dir.LocList(loc);
		WHILE li # NIL DO
			IF rung = 0 THEN subsystem := li.name END;
			IF (rung = 0) OR ((rung = 1) & ContainsDocuments (li.name)) THEN
				ScanSubsystems(loc.This(li.name$), TRUE, rung + 1)
			END;
			li := li.next
		END;
	END ScanSubsystems;

END ArcEnumDocs.Do