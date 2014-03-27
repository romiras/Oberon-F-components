MODULE CairoTextsWin32;


(*
	Copyright (C) 2012  Romiras
	License: LGPL v 2.1##=>

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

##<=
*)

	IMPORT WinApi, Lib := LibsCairo, LibWin32 := LibsCairoWin32, M := CairoModels, T := CairoTexts;

	TYPE
		CrFontFaceWin32* = POINTER TO RECORD (T.CrFontFace) END;
		CrScaledFontWin32* = POINTER TO RECORD (T.CrScaledFont) END;
		
	PROCEDURE (fw: CrFontFaceWin32) CreateForLogFont* (logfont: WinApi.PtrLOGFONTW), NEW;
	BEGIN
		fw.SetFontFace(LibWin32.cairo_win32_font_face_create_for_logfontw(logfont))
	END CreateForLogFont;
	
	PROCEDURE (fw: CrFontFaceWin32) CreateForHFont* (font: WinApi.HFONT), NEW;
	BEGIN
		fw.SetFontFace(LibWin32.cairo_win32_font_face_create_for_hfont(font))
	END CreateForHFont;

	
	PROCEDURE (sf: CrScaledFontWin32) SelectFont* (HDC: LibWin32.Handle): Lib.cairo_status_t, NEW;
	BEGIN
		RETURN LibWin32.cairo_win32_scaled_font_select_font(sf.fScaledFont, HDC)
	END SelectFont;
	
	PROCEDURE (sf: CrScaledFontWin32) DoneFont* (HDC: LibWin32.Handle), NEW;
	BEGIN
		LibWin32.cairo_win32_scaled_font_done_font(sf.fScaledFont)
	END DoneFont;
	
	PROCEDURE (sf: CrScaledFontWin32) GetMetricsFactor* (): Lib.Real, NEW;
	BEGIN
		RETURN LibWin32.cairo_win32_scaled_font_get_metrics_factor(sf.fScaledFont)
	END GetMetricsFactor;
	
	PROCEDURE (sf: CrScaledFontWin32) GetLogicalToDevice* (VAR logical_to_device: Lib.cairo_matrix_t), NEW;
	BEGIN
		LibWin32.cairo_win32_scaled_font_get_logical_to_device(sf.fScaledFont, logical_to_device)
	END GetLogicalToDevice;
	
	PROCEDURE (sf: CrScaledFontWin32) GetDeviceToLogical* (VAR device_to_logical: Lib.cairo_matrix_t), NEW;
	BEGIN
		LibWin32.cairo_win32_scaled_font_get_device_to_logical(sf.fScaledFont, device_to_logical)
	END GetDeviceToLogical;

END CairoTextsWin32.
