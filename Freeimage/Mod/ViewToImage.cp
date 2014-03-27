MODULE FreeimageViewToImage;
	(* Date: 24 Dec. 2006 *)
	(* Author: Necati Ecevit   *)
	(* Converts any Views.View to FIBITMAP or stores it to a file *)

	IMPORT Lib := LibsFreeImage, Utils := FreeimageUtils, UtilsWin := FreeimageUtilsWin,
		Ports, Models, Views, Bitmaps,
		HostPorts, HostWindows, WinApi;

TYPE
		FIBITMAP = Lib.FIBITMAP;
(*	
	PROCEDURE InitPort (VAR p: HostPorts.Port; w, h, unit: INTEGER);
		VAR dc: WinApi.HDC;
	BEGIN
		NEW(p);
		p.Init(unit, Ports.screen);
		p.SetSize(w, h);
	END InitPort;
*)

(* idea from HostBitmaps.TurnToBitmap *)
	PROCEDURE ViewToFImage* (v: Views.View; bits: INTEGER): FIBITMAP;
		VAR p: HostPorts.Port; dc, bdc: WinApi.HDC; bm: WinApi.HBITMAP; res, w, h: INTEGER; obj: FIBITMAP;
	BEGIN
		ASSERT(v # NIL, 20);
		ASSERT((bits = 0) OR (bits = 8) OR (bits = 24) OR (bits = 32), 21);
		dc := WinApi.GetDC(HostWindows.main);
		bdc := WinApi.CreateCompatibleDC(dc);
		res := WinApi.SetBkMode(bdc, WinApi.TRANSPARENT);
		v.context.GetSize(w, h);
		w := w DIV HostWindows.unit; h := h DIV HostWindows.unit;
		bm := WinApi.CreateCompatibleBitmap(dc, w, h);
		res := WinApi.SelectObject(bdc, bm);
		NEW(p);
		p.Init(HostWindows.unit, Ports.printer); (* Ports.screen *)
		p.SetSize(w, h);
		p.SetDC(bdc, 0);
		Bitmaps.Paint(v, p, w, h, HostWindows.unit);
		obj := UtilsWin.HBitmapToFImage(bm);
		res := WinApi.DeleteDC(bdc);
		RETURN obj;
	END ViewToFImage;

	PROCEDURE StoreViewAsImage* (v: Views.View; fname: ARRAY OF CHAR; bits, flags: INTEGER): BOOLEAN;
		VAR res: BOOLEAN;
			tim, im: FIBITMAP;
	BEGIN
		IF v = NIL THEN RETURN FALSE END;
		tim := ViewToFImage(v, bits);
		IF tim = NIL THEN RETURN FALSE END;
		CASE bits OF
		  8: im:=Lib.ConvertTo8Bits(tim);
		| 24: im:=Lib.ConvertTo24Bits(tim);
		| 32: im:=Lib.ConvertTo32Bits(tim);
		ELSE
			im:=Lib.ConvertTo24Bits(tim);
		END;
		IF im = NIL THEN
			RETURN FALSE
		ELSE
			res := Utils.Save(im, fname, flags);
			Lib.Unload(im);
			RETURN res
		END
	END StoreViewAsImage;

END FreeimageViewToImage.
