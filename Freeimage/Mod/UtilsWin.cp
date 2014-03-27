MODULE FreeimageUtilsWin;
(* Author: Necati Ecevit *)

	IMPORT SYSTEM, WinApi, Lib := LibsFreeImage, LibWin32 := LibsFreeImageWin32;
	
	TYPE
		FIBITMAP = Lib.FIBITMAP;

	(* convert HBITMAP to FIBITMAP *)
	PROCEDURE HBitmapToFImage* (hbmp: WinApi.HBITMAP): FIBITMAP;
		VAR dib: FIBITMAP;
			res, succes: INTEGER;
			DC: WinApi.HDC;
			bm: WinApi.BITMAP;
	BEGIN
		IF hbmp # 0 THEN
			(* get information about the bitmap *)
			res := WinApi.GetObjectW(hbmp, SIZE(WinApi.BITMAP), SYSTEM.ADR(bm));
			(* create the image*)
			dib := Lib.Allocate(bm.bmWidth, bm.bmHeight, bm.bmBitsPixel, 0, 0, 0);
			ASSERT(dib # NIL, 20);
			(* create the device context for the bitmap *)
			DC := WinApi.GetDC(0);

			(* copy the pixels *)
			succes := WinApi.GetDIBits(DC, (* handle to DC *)
				hbmp, (* handle to Bitmap *)
				0, (* first scan line *)
				Lib.GetHeight(dib), (* number of scan lines to copy *)
				Lib.GetBits(dib), (* array for bitmap bits *)
				LibWin32.GetInfo(dib)^, (* bitmap data buffer *)
				WinApi.DIB_RGB_COLORS (* RGB *)
			);

			res := WinApi.ReleaseDC(0, DC);
			IF succes = 0 THEN RETURN NIL ELSE RETURN dib END
		ELSE
			RETURN NIL
		END
	END HBitmapToFImage;

	(* convert FIMAGE to HBITMAP *)
	PROCEDURE FImageToHBitmap* (dib: FIBITMAP): WinApi.HBITMAP;
		VAR DC: WinApi.HDC;
			res: INTEGER;
			bm: WinApi.HBITMAP;
	BEGIN
		IF dib # NIL THEN
			DC := WinApi.GetDC(0);
			bm := WinApi.CreateDIBitmap(DC,
			LibWin32.GetInfoHeader(dib)^, WinApi.CBM_INIT,
			Lib.GetBits(dib), LibWin32.GetInfo(dib)^, WinApi.DIB_RGB_COLORS);
			res := WinApi.ReleaseDC(0, DC);
			RETURN bm
		ELSE
			RETURN 0
		END
	END FImageToHBitmap;
	
END FreeimageUtilsWin.