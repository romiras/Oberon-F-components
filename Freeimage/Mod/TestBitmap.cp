MODULE FreeimageTestBitmap;
	(* Date: 11 Dec. 2006 *)
	(* Author: Necati Ecevit *)
	(* Contributor: Romiras *)
	(* This module is a test for HBITMAP to FBITMAP conversion*)

	IMPORT WinApi, Lib := LibsFreeImage, Utils := FreeimageUtils, UtilsWin := FreeimageUtilsWin,
		(*Window := FreeWindow,*) Files, HostFiles, Out, Dialog;
	
	TYPE
		FIBITMAP = Lib.FIBITMAP;
	
	PROCEDURE GetFullPath (OUT full: Files.Name): BOOLEAN;
		VAR fname: Files.Name;
			loc: Files.Locator;
	BEGIN
		loc := NIL;
		Dialog.GetIntSpec("*", loc, fname);
		IF loc = NIL THEN
			full := ""
		ELSE
			full := loc(HostFiles.Locator).path$ + "\" + fname$
		END;
		RETURN loc # NIL
	END GetFullPath;
	
	PROCEDURE InspectImage (tdib: FIBITMAP; IN name: ARRAY OF CHAR);
	BEGIN
		Out.String(name$); Out.String(": Image Loaded .."); Out.Ln();
		
		Out.String('WxH x BPP: '); Out.Int(Lib.GetWidth(tdib), 0);
		Out.Int(Lib.GetHeight(tdib), 5); Out.Int(Lib.GetBPP(tdib),5); Out.Ln;
		
		(* show it *)
		(*Window.ShowPicture(tdib, name$);*)
	END InspectImage;
	
	PROCEDURE SaveHBitmapAsImage(bm: WinApi.HBITMAP; IN name: ARRAY OF CHAR): BOOLEAN;
		VAR tdib: FIBITMAP; res: INTEGER; saved: BOOLEAN;
	BEGIN
		(* convert to FreeImage FIBTMAP *)
		tdib := UtilsWin.HBitmapToFImage(bm);
		IF tdib # NIL THEN			
			InspectImage(tdib, name$);
			
			(* save it with the supported file formats*)
			saved := Utils.Save(tdib, name$, 0);
			
			(* release resources *)
			Lib.Unload(tdib)
		ELSE
			saved := FALSE
		END;
		RETURN saved
	END SaveHBitmapAsImage;
	
		(* Open a bitmap (HBITMAP) image file, convert to FIBITMAP, show it and save it as jpeg, png ... *)
	PROCEDURE Open*;
		VAR fname: Files.Name;
			bm: WinApi.HBITMAP;
			res: INTEGER;
	BEGIN
		IF GetFullPath(fname) THEN
			(* load the windows bitmap file *)
			bm := WinApi.LoadImageW(0, Utils.StringAddr(fname), WinApi.IMAGE_BITMAP, 0, 0, 		ORD(WinApi.LR_LOADFROMFILE));
			IF bm # 0 THEN
				IF SaveHBitmapAsImage(bm, "test.jpeg") THEN (* test.png, test.gif ... *)
					Out.String(fname$); Out.String(": is saved as test.jpeg"); Out.Ln()
				ELSE
					Out.String(fname$); Out.String(": Image NOT saved .."); Out.Ln()
				END;
				res := WinApi.DeleteObject(bm)
			ELSE
				Out.String(fname$); Out.String(": Image NOT Loaded .."); Out.Ln()
			END;
			Out.Int(bm, 0); Out.Ln
		ELSE
			Out.String("Open Cancelled .."); Out.Ln();
			RETURN
		END;
	END Open;

END FreeimageTestBitmap.
