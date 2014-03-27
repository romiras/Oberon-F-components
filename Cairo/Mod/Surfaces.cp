MODULE CairoSurfaces;


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

	IMPORT Lib := LibsCairo, M := CairoModels;

	TYPE
		PdfSurface* = POINTER TO RECORD (M.DocumentSurface) END;
		PsSurface* = POINTER TO RECORD (M.DocumentSurface) END;
		SvgSurface* = POINTER TO RECORD (M.DocumentSurface) END;
		
		CrSvgVersion* = Lib.cairo_svg_version_t;

	(* PdfSurface *)

	PROCEDURE (PDFs: PdfSurface) Create*  (Afilename: M.SString; Awidth, Aheight: M.Real),	NEW;
		VAR Surface: Lib.cairo_surface_t;
	BEGIN
		Surface := Lib.cairo_pdf_surface_create (Afilename, Awidth, Aheight);
		PDFs.Assign (Surface)
	END Create;

	PROCEDURE (PDFs: PdfSurface) SetSize*  (Awidth, Aheight: M.Real);
	BEGIN
		Lib.cairo_pdf_surface_set_size (PDFs.surface, Awidth, Aheight)
	END SetSize;

	PROCEDURE (PDFs: PdfSurface) CreateForStream  (write_func: M.CrWriteProc; closure: M.Data; width_in_points, height_in_points: M.Real): Lib.cairo_surface_t, NEW;
	BEGIN
		RETURN Lib.cairo_pdf_surface_create_for_stream (write_func, closure, width_in_points, height_in_points)
	END CreateForStream;


	(* PsSurface *)

	PROCEDURE (PSs: PsSurface) Create*  (Afilename: M.SString; Awidth, Aheight: M.Real),	NEW;
		VAR Surface: Lib.cairo_surface_t;
	BEGIN
     (*Inherited Create;*)
		Surface := Lib.cairo_ps_surface_create (Afilename, Awidth, Aheight);
		PSs.Assign (Surface)
	END Create;

	PROCEDURE (PSs: PsSurface) SetSize*  (Awidth, Aheight: M.Real);
	BEGIN
		Lib.cairo_ps_surface_set_size (PSs.surface, Awidth, Aheight)
	END SetSize;

	PROCEDURE (PSs: PsSurface) CreateForStream  (write_func: M.CrWriteProc; closure: M.Data; width_in_points, height_in_points: M.Real): Lib.cairo_surface_t, NEW;
	BEGIN
		RETURN Lib.cairo_ps_surface_create_for_stream (write_func, closure, width_in_points, height_in_points)
	END CreateForStream;

	PROCEDURE (PSs: PsSurface) DSCComment* (comment: M.SString),	NEW;
	BEGIN
		Lib.cairo_ps_surface_dsc_comment (PSs.surface, comment)
	END DSCComment;

	PROCEDURE (PSs: PsSurface) DSCBeginSetup*,	NEW;
	BEGIN
		Lib.cairo_ps_surface_dsc_begin_setup (PSs.surface)
	END DSCBeginSetup;

	PROCEDURE (PSs: PsSurface) DSCBeginPageSetup*,	NEW;
	BEGIN
		Lib.cairo_ps_surface_dsc_begin_page_setup (PSs.surface)
	END DSCBeginPageSetup;


	(* SvgSurface *)

	PROCEDURE (SVGs: SvgSurface) Create* (Afilename: M.SString; Awidth, Aheight: M.Real),	NEW;
	VAR Surface: Lib.cairo_surface_t;
	BEGIN
		(*Inherited Create;*)
		Surface := Lib.cairo_svg_surface_create (Afilename, Awidth, Aheight);
		SVGs.Assign(Surface)
	END Create;

	PROCEDURE (SVGs: SvgSurface) CreateForStream (write_func: M.CrWriteProc; closure: M.Data; width_in_points, height_in_points: M.Real): Lib.cairo_surface_t, NEW;
	BEGIN
		RETURN Lib.cairo_svg_surface_create_for_stream (write_func, closure, width_in_points, height_in_points)
	END CreateForStream;

	PROCEDURE (SVGs: SvgSurface) RestrictToVersion* (version: CrSvgVersion),	NEW;
	BEGIN
		Lib.cairo_svg_surface_restrict_to_version (SVGs.surface, version)
	END RestrictToVersion;

	PROCEDURE (SVGs: SvgSurface) GetVersions* (VAR versions: CrSvgVersion; VAR num_versions: M.Integer),	NEW;
	BEGIN
		Lib.cairo_svg_get_versions (versions, num_versions)
	END GetVersions;

	PROCEDURE (SVGs: SvgSurface) VersionToString* (version: CrSvgVersion): M.SString,	NEW;
	BEGIN
		RETURN Lib.cairo_svg_version_to_string (version)
	END VersionToString;
	

	PROCEDURE NewDocumentSurface* (): M.DocumentSurface;
		VAR surface: M.DocumentSurface;
	BEGIN
		NEW(surface);
		RETURN surface
	END NewDocumentSurface;

	PROCEDURE NewMemBufSurface* (): M.MemBufSurface;
		VAR surface: M.MemBufSurface;
	BEGIN
		NEW(surface);
		RETURN surface
	END NewMemBufSurface;

	PROCEDURE NewDrawableSurface* (): M.DrawableSurface;
		VAR surface: M.DrawableSurface;
	BEGIN
		NEW(surface);
		RETURN surface
	END NewDrawableSurface;

END CairoSurfaces.

