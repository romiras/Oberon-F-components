MODULE FreeimageUtils;
(* Date: 11 Dec. 2006 *)
(* Author: Necati Ecevit   *)

	(* Some High level wrapper procedures Blackbox.*)
	IMPORT SYSTEM, Lib := LibsFreeImage;

	TYPE
		FIBITMAP = Lib.FIBITMAP;
		RGBQUAD = Lib.RGBQUAD;
		Histogram* = ARRAY 256 OF INTEGER;
		
		PChar* (*SStringPtr*) = Lib.PChar;
		PUChar* (*StringPtr*) = Lib.PCharU;
		
	VAR
		name: ARRAY 128 OF SHORTCHAR;
		
	PROCEDURE SStringAddr* (IN x: ARRAY OF SHORTCHAR): PChar;
	BEGIN
		RETURN SYSTEM.VAL(PChar, SYSTEM.ADR(x[0]))
	END SStringAddr;
	
	PROCEDURE StringAddr* (IN x: ARRAY OF CHAR): PUChar;
	BEGIN
		RETURN SYSTEM.VAL(PUChar, SYSTEM.ADR(x[0]))
	END StringAddr;

		(***** Wrapper functions ***)
	PROCEDURE Version* (VAR ver: ARRAY OF CHAR);
		VAR s: Lib.PChar;
	BEGIN
		s := Lib.GetVersion();
		ver := LONG(s^)
	END Version;

	PROCEDURE CopyRight* (VAR ver: ARRAY OF CHAR);
		VAR s: Lib.PChar;
	BEGIN
		s := Lib.GetCopyrightMessage();
		ver := LONG(s^)
	END CopyRight;

	PROCEDURE PutPixel32* (dib: FIBITMAP; x, y: INTEGER; r, g, b, a: INTEGER);
		VAR color: RGBQUAD;
	BEGIN
		color.rgbRed := SHORT(CHR(r));
		color.rgbGreen := SHORT(CHR(g));
		color.rgbBlue := SHORT(CHR(b));
		color.rgbReserved := SHORT(CHR(b));
		Lib.SetPixelColor(dib, x, y, color)
	END PutPixel32;

	PROCEDURE GetPixel32* (dib: FIBITMAP; x, y: INTEGER; VAR r, g, b, a: INTEGER);
		VAR color: RGBQUAD;
	BEGIN
		Lib.GetPixelColor(dib, x, y, color);
		r := ORD(color.rgbRed);
		g := ORD(color.rgbGreen);
		b := ORD(color.rgbBlue);
		a := ORD(color.rgbReserved)
	END GetPixel32;


	PROCEDURE PutPixel24* (dib: FIBITMAP; x, y: INTEGER; r, g, b: INTEGER);
		VAR color: RGBQUAD;
	BEGIN
		color.rgbRed := SHORT(CHR(r));
		color.rgbGreen := SHORT(CHR(g));
		color.rgbBlue := SHORT(CHR(b));
		Lib.SetPixelColor(dib, x, y, color)
	END PutPixel24;

	PROCEDURE GetPixel24* (dib: FIBITMAP; x, y: INTEGER; VAR r, g, b: INTEGER);
		VAR color: RGBQUAD;
	BEGIN
		Lib.GetPixelColor(dib, x, y, color);
		r := ORD(color.rgbRed);
		g := ORD(color.rgbGreen);
		b := ORD(color.rgbBlue)
	END GetPixel24;


	PROCEDURE GetHistogram (dib: FIBITMAP; channel: INTEGER; VAR hist: Histogram): BOOLEAN;
		VAR res: BOOLEAN;
	BEGIN
		res := Lib.GetHistogram(dib, hist, channel);
		RETURN res
	END GetHistogram;

	PROCEDURE ConvertTo24Bits* (dib: FIBITMAP): FIBITMAP;
		VAR img: FIBITMAP;
	BEGIN
		IF Lib.GetBPP(dib) # 24 THEN
			img := Lib.ConvertTo24Bits(dib)
		ELSE
			img := dib
		END;
		RETURN img
	END ConvertTo24Bits;

	PROCEDURE ConvertTo32Bits* (dib: FIBITMAP): FIBITMAP;
		VAR img: FIBITMAP;
	BEGIN
		IF Lib.GetBPP(dib) # 32 THEN
			img := Lib.ConvertTo32Bits(dib)
		ELSE
			img := dib
		END;
		RETURN img
	END ConvertTo32Bits;
	
	PROCEDURE FlipVertical* (dib: FIBITMAP): BOOLEAN;
	BEGIN
		RETURN Lib.FlipVertical(dib)
	END FlipVertical;

	PROCEDURE FlipHorizontal* (dib: FIBITMAP): BOOLEAN;
	BEGIN
		RETURN Lib.FlipHorizontal(dib)
	END FlipHorizontal;

	(* load file fname as FIBITMAP *)
	PROCEDURE Load* (fname: ARRAY OF CHAR): FIBITMAP;
		VAR
			res: BOOLEAN;
			type: Lib.IMAGE_FORMAT;
			tdib: Lib.FIBITMAP;
	BEGIN
		type := Lib.GetFileTypeU(fname, 0);
		IF type = Lib.FIF_UNKNOWN THEN
			(* Out.String("Unkonwn Format: "); Out.Int(type, 0); Out.Ln; *)
			type := Lib.GetFIFFromFilenameU(fname)
		END;
		IF (type # Lib.FIF_UNKNOWN) & Lib.FIFSupportsReading(type) THEN
			(* Out.Int(type, 0); Out.String(" Supported reading: "); Out.Ln; *)
			tdib := Lib.LoadU(type, fname, 0);
			RETURN tdib
		ELSE
			RETURN NIL
		END
	END Load;

	
	PROCEDURE GetFileType* (fname: ARRAY OF CHAR): INTEGER;
		VAR fif: Lib.IMAGE_FORMAT;
	BEGIN
		name := SHORT(fname$);
		fif := Lib.GetFileType( SStringAddr(name) , 0);
		RETURN fif;
	END GetFileType;
	
	(* save FITMAP image to a file *)
	PROCEDURE Save* (dib: FIBITMAP; fname: ARRAY OF CHAR; flags: INTEGER): BOOLEAN;
		VAR	 res, cansave, success: BOOLEAN;
			fif: Lib.IMAGE_FORMAT;
			type: Lib.IMAGE_TYPE;
			bpp: INTEGER;
	BEGIN
		success := FALSE;
		fif := Lib.GetFIFFromFilenameU(fname);
		IF fif # Lib.FIF_UNKNOWN THEN
			type := Lib.GetImageType(dib);
			IF type = Lib.FIT_BITMAP THEN
				bpp := Lib.GetBPP(dib);
				cansave := Lib.FIFSupportsWriting(fif) & Lib.FIFSupportsExportBPP(fif, bpp)
			ELSE
				cansave := Lib.FIFSupportsExportType(fif, type)
			END;
			IF cansave THEN
				success := Lib.SaveU(fif, dib, fname, flags)
			END
		END;
		RETURN success
	END Save;

END FreeimageUtils.
