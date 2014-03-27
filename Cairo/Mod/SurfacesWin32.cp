MODULE CairoSurfacesWin32;


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

	IMPORT Lib := LibsCairo, LibWin32 := LibsCairoWin32, M := CairoModels;

	TYPE
		GdiSurface* = POINTER TO RECORD (M.DrawableSurface) END;

	(* GdiSurface *)

	PROCEDURE (sGdi: GdiSurface) Create* (HDC: LibWin32.Handle), NEW;
	VAR Surface: M.CrSurface;
	BEGIN
		Surface := LibWin32.cairo_win32_surface_create (HDC);
		sGdi.Assign(Surface)
	END Create;

	PROCEDURE (sGdi: GdiSurface) CreateWithDDB* (HDC: LibWin32.Handle; format: M.CrFormat; width, height: Lib.Integer), NEW;
	VAR Surface: M.CrSurface;
	BEGIN
		Surface := LibWin32.cairo_win32_surface_create_with_ddb (HDC, format, width, height);
		sGdi.Assign(Surface)
	END CreateWithDDB;

	PROCEDURE (sGdi: GdiSurface) CreateWithDIB* (format: M.CrFormat; width, height: Lib.Integer), NEW;
	VAR Surface: M.CrSurface;
	BEGIN
		Surface := LibWin32.cairo_win32_surface_create_with_dib (format, width, height);
		sGdi.Assign(Surface)
	END CreateWithDIB;

	PROCEDURE (sGdi: GdiSurface) GetDC* (): LibWin32.Handle, NEW;
	BEGIN
		RETURN LibWin32.cairo_win32_surface_get_dc (sGdi.surface)
	END GetDC;

	PROCEDURE (sGdi: GdiSurface) GetImage* (): M.CrSurface, NEW;
	BEGIN
		RETURN LibWin32.cairo_win32_surface_get_image (sGdi.surface)
	END GetImage;
	
END CairoSurfacesWin32.
