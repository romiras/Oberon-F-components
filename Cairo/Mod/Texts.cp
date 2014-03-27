MODULE CairoTexts;


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

	IMPORT Api := LibsCairo, M := CairoModels;

	TYPE
		FontOptions* = POINTER TO RECORD
			fFOpt-: Api.cairo_font_options_t
		END;
		
		FontFace* = POINTER TO EXTENSIBLE RECORD (M.Base)
			fFontFace-: Api.cairo_font_face_t
		END;
		
		ScaledFont* = POINTER TO EXTENSIBLE RECORD (M.Base)
			fScaledFont-: Api.cairo_scaled_font_t;
			fFontFace-: FontFace;
			fFontMatrix-,
			fCTM-: M.CrMatrix;
			foptions-: Api.cairo_font_options_t;
		END;
		
		(* This interface is for dealing with text as text, not caring about the font object inside the the cairo_t. *)
		Text* = POINTER TO RECORD
			cr: Api.cairo_t;
			(*fFontOpt-: FontOptions;
			fFontFace-: FontFace;
			fScFont-: ScaledFont;*)
		END;

	(* FontOptions *)

	PROCEDURE (fo: FontOptions) Create*, NEW;
	BEGIN
		fo.fFOpt := Api.cairo_font_options_create ()
	END Create;

	PROCEDURE (fo: FontOptions) Destroy*, NEW;
	BEGIN
		Api.cairo_font_options_destroy (fo.fFOpt)
	END Destroy;

	PROCEDURE (fo: FontOptions) Copy* (original: Api.cairo_font_options_t): Api.cairo_font_options_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_copy (original)
	END Copy;

	PROCEDURE (fo: FontOptions) Status* (): M.Status, NEW;
	BEGIN
		RETURN Api.cairo_font_options_status (fo.fFOpt)
	END Status;

	PROCEDURE (fo: FontOptions) Merge* (other: Api.cairo_font_options_t), NEW;
	BEGIN
		Api.cairo_font_options_merge (fo.fFOpt, other)
	END Merge;

	PROCEDURE (fo: FontOptions) Equal* (other: Api.cairo_font_options_t): Api.cairo_bool_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_equal (fo.fFOpt, other)
	END Equal;

	PROCEDURE (fo: FontOptions) Hash* (): Api.Integer, NEW;
	BEGIN
		RETURN Api.cairo_font_options_hash (fo.fFOpt)
	END Hash;

	PROCEDURE (fo: FontOptions) SetAntialias* (antialias: Api.cairo_antialias_t), NEW;
	BEGIN
		Api.cairo_font_options_set_antialias (fo.fFOpt, antialias)
	END SetAntialias;

	PROCEDURE (fo: FontOptions) GetAntialias* (): Api.cairo_antialias_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_get_antialias (fo.fFOpt)
	END GetAntialias;

	PROCEDURE (fo: FontOptions) SetSubpixelOrder* (subpixel_order: Api.cairo_subpixel_order_t), NEW;
	BEGIN
		Api.cairo_font_options_set_subpixel_order (fo.fFOpt, subpixel_order)
	END SetSubpixelOrder;

	PROCEDURE (fo: FontOptions) GetSubpixelOrder* (): Api.cairo_subpixel_order_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_get_subpixel_order (fo.fFOpt)
	END GetSubpixelOrder;

	PROCEDURE (fo: FontOptions) SetHintStyle* (hint_style: Api.cairo_hint_style_t), NEW;
	BEGIN
		Api.cairo_font_options_set_hint_style (fo.fFOpt, hint_style)
	END SetHintStyle;

	PROCEDURE (fo: FontOptions) GetHintStyle* (): Api.cairo_hint_style_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_get_hint_style (fo.fFOpt)
	END GetHintStyle;

	PROCEDURE (fo: FontOptions) SetHintMetrics* (hint_metrics: Api.cairo_hint_metrics_t), NEW;
	BEGIN
		Api.cairo_font_options_set_hint_metrics (fo.fFOpt, hint_metrics)
	END SetHintMetrics;

	PROCEDURE (fo: FontOptions) GetHintMetrics* (): Api.cairo_hint_metrics_t, NEW;
	BEGIN
		RETURN Api.cairo_font_options_get_hint_metrics (fo.fFOpt)
	END GetHintMetrics;


	(* FontFace *)

	PROCEDURE (ff: FontFace) Create*, NEW;
	BEGIN
		(*Inherited Create;*)
		ff.fFontFace := NIL
	END Create;

	PROCEDURE (ff: FontFace) SetFontFace* (fontface: Api.cairo_font_face_t), NEW;
	BEGIN
		ff.fFontFace := fontface
	END SetFontFace;

	PROCEDURE (ff: FontFace) RefPtr*(): Api.cairo_font_face_t, NEW;
	BEGIN
		RETURN ff.fFontFace
	END RefPtr;

	PROCEDURE (ff: FontFace) Destroy*;
	BEGIN
		Api.cairo_font_face_destroy (ff.fFontFace)
	END Destroy;

	PROCEDURE (ff: FontFace) GetStatus* (): M.Status;
	BEGIN
		RETURN Api.cairo_font_face_status (ff.fFontFace)
	END GetStatus;

	(*
	PROCEDURE (ff: FontFace) Reference* (): Api.cairo_font_face_t, NEW;
	BEGIN
		RETURN Api.cairo_font_face_reference (ff.fFontFace)
	END Reference;

	PROCEDURE (ff: FontFace) GetReferenceCount*(): Api.Integer;
	BEGIN
		RETURN Api.cairo_font_face_get_reference_count (ff.fFontFace)
	END GetReferenceCount;
	*)

	PROCEDURE (ff: FontFace) GetUserData* (key: Api.cairo_user_data_key_t): M.Data;
	BEGIN
		RETURN Api.cairo_font_face_get_user_data (ff.fFontFace, key)
	END GetUserData;

	PROCEDURE (ff: FontFace) SetUserData* (key: Api.cairo_user_data_key_t;
		user_data: M.Data; destroy_func: Api.cairo_destroy_func_t): M.Status;
	BEGIN
		RETURN Api.cairo_font_face_set_user_data (ff.fFontFace, key, user_data, destroy_func)
	END SetUserData;

	PROCEDURE (ff: FontFace) GetType* (): Api.cairo_font_type_t, NEW;
	BEGIN
		RETURN Api.cairo_font_face_get_type (ff.fFontFace)
	END GetType;


	(* ScaledFont *)

	PROCEDURE (sf: ScaledFont) Create* (
		AFontFace: FontFace;
		AFontMatrix,
		ACTM: M.CrMatrix;
		Aoptions: Api.cairo_font_options_t
		), NEW;
	BEGIN
		(*Inherited Create;*)
		sf.fFontFace := AFontFace;
		sf.fFontMatrix := AFontMatrix;
		sf.fCTM := ACTM;
		sf.foptions := Aoptions;
		sf.fScaledFont := Api.cairo_scaled_font_create (sf.fFontFace.RefPtr(), sf.fFontMatrix, sf.fCTM, sf.foptions)
	END Create;

	PROCEDURE (sf: ScaledFont) RefPtr*(): Api.cairo_scaled_font_t, NEW;
	BEGIN
		RETURN sf.fScaledFont
	END RefPtr;
	
	(*
	PROCEDURE (sf: ScaledFont) Reference*(): Api.cairo_scaled_font_t, NEW;
	BEGIN
		RETURN Api.cairo_scaled_font_reference (sf.fScaledFont)
	END Reference;

	PROCEDURE (sf: ScaledFont) GetReferenceCount*(): Api.Integer;
	BEGIN
		RETURN Api.cairo_scaled_font_get_reference_count (sf.fScaledFont)
	END GetReferenceCount;
	*)

	PROCEDURE (sf: ScaledFont) Destroy*;
	BEGIN
		Api.cairo_scaled_font_destroy (sf.fScaledFont);
	END Destroy;

	PROCEDURE (sf: ScaledFont) GetStatus* (): M.Status;
	BEGIN
		RETURN Api.cairo_scaled_font_status (sf.fScaledFont)
	END GetStatus;

	PROCEDURE (sf: ScaledFont) GetUserData* (key: Api.cairo_user_data_key_t): M.Data;
	BEGIN
		RETURN Api.cairo_scaled_font_get_user_data (sf.fScaledFont, key)
	END GetUserData;

	PROCEDURE (sf: ScaledFont) SetUserData* (key: Api.cairo_user_data_key_t;
		user_data: M.Data; destroy_func: Api.cairo_destroy_func_t): M.Status;
	BEGIN
		RETURN Api.cairo_scaled_font_set_user_data (sf.fScaledFont, key, user_data, destroy_func)
	END SetUserData;

	PROCEDURE (sf: ScaledFont) GetExtents* (VAR extents: Api.cairo_font_extents_t), NEW;
	BEGIN
		Api.cairo_scaled_font_extents (sf.fScaledFont, extents)
	END GetExtents;

	PROCEDURE (sf: ScaledFont) GetTextExtents* (text: M.SString; VAR extents: Api.cairo_text_extents_t), NEW;
	BEGIN
		Api.cairo_scaled_font_text_extents (sf.fScaledFont, text, extents)
	END GetTextExtents;

	PROCEDURE (sf: ScaledFont) GetGlyphExtents* (
		scaled_font: Api.cairo_scaled_font_t;
		glyphs: Api.cairo_glyph_t;
		num_glyphs: Api.Integer;
		VAR extents: Api.cairo_text_extents_t), NEW;
	BEGIN
		Api.cairo_scaled_font_glyph_extents (sf.fScaledFont, glyphs, num_glyphs, extents)
	END GetGlyphExtents;

	PROCEDURE (sf: ScaledFont) GetFontFace* (): Api.cairo_font_face_t, NEW;
	BEGIN
		RETURN Api.cairo_scaled_font_get_font_face (sf.fScaledFont)
	END GetFontFace;

	PROCEDURE (sf: ScaledFont) GetFontMatrix* (VAR font_matrix: M.CrMatrix), NEW;
	BEGIN
		Api.cairo_scaled_font_get_font_matrix (sf.fScaledFont, font_matrix)
	END GetFontMatrix;

	PROCEDURE (sf: ScaledFont) GetCTM* (VAR ctm: M.CrMatrix), NEW;
	BEGIN
		Api.cairo_scaled_font_get_ctm (sf.fScaledFont, ctm)
	END GetCTM;

	PROCEDURE (sf: ScaledFont) GetFontOptions* (VAR options: Api.cairo_font_options_t), NEW;
	BEGIN
		Api.cairo_scaled_font_get_font_options (sf.fScaledFont, options)
	END GetFontOptions;


	(* Text *)

	PROCEDURE (ct: Text) ConnectTo* (ctx: M.Context), NEW;
	BEGIN
		ct.cr := ctx.cr
	END ConnectTo;

	PROCEDURE (ct: Text) SelectFontFace* (family: M.SString; slant: Api.cairo_font_slant_t; weight: Api.cairo_font_weight_t), NEW;
	BEGIN
		Api.cairo_select_font_face (ct.cr, family, slant, weight)
	END SelectFontFace;

	PROCEDURE (ct: Text) SetFontSize* (size: M.Real), NEW;
	BEGIN
		Api.cairo_set_font_size (ct.cr, size)
	END SetFontSize;

	PROCEDURE (ct: Text) SetFontMatrix* (matrix: M.CrMatrix), NEW;
	BEGIN
		Api.cairo_set_font_matrix (ct.cr, matrix)
	END SetFontMatrix;

	PROCEDURE (ct: Text) GetFontMatrix* (VAR matrix: M.CrMatrix), NEW;
	BEGIN
		Api.cairo_get_font_matrix (ct.cr, matrix)
	END GetFontMatrix;

	PROCEDURE (ct: Text) SetFontOptions* (options: Api.cairo_font_options_t), NEW;
	BEGIN
		Api.cairo_set_font_options (ct.cr, options)
	END SetFontOptions;

	PROCEDURE (ct: Text) GetFontOptions* (VAR options: Api.cairo_font_options_t), NEW;
	BEGIN
		Api.cairo_get_font_options (ct.cr, options)
	END GetFontOptions;

	PROCEDURE (ct: Text) SetFontFace* (font_face: Api.cairo_font_face_t), NEW;
	BEGIN
		Api.cairo_set_font_face (ct.cr, font_face)
	END SetFontFace;

	PROCEDURE (ct: Text) GetFontFace* (): Api.cairo_font_face_t, NEW;
	BEGIN
		RETURN Api.cairo_get_font_face (ct.cr)
	END GetFontFace;

	PROCEDURE (ct: Text) SetScaledFont* (scaled_font: Api.cairo_scaled_font_t), NEW;
	BEGIN
		Api.cairo_set_scaled_font (ct.cr, scaled_font)
	END SetScaledFont;

	PROCEDURE (ct: Text) GetScaledFont* (): Api.cairo_scaled_font_t, NEW;
	BEGIN
		RETURN Api.cairo_get_scaled_font (ct.cr)
	END GetScaledFont;

	PROCEDURE (ct: Text) ShowText* (text: M.SString), NEW;
	BEGIN
		Api.cairo_show_text (ct.cr, text)
	END ShowText;

	PROCEDURE (ct: Text) ShowGlyphs* (glyphs: Api.cairo_glyph_t; num_glyphs: Api.Integer), NEW;
	BEGIN
		Api.cairo_show_glyphs (ct.cr, glyphs, num_glyphs)
	END ShowGlyphs;

	PROCEDURE (ct: Text) TextPath* (text: M.SString), NEW;
	BEGIN
		Api.cairo_text_path (ct.cr, text)
	END TextPath;

	PROCEDURE (ct: Text) GlyphPath* (glyphs: Api.cairo_glyph_t; num_glyphs: Api.Integer), NEW;
	BEGIN
		Api.cairo_glyph_path (ct.cr, glyphs, num_glyphs)
	END GlyphPath;

	PROCEDURE (ct: Text) GetTextExtents* (text: M.SString; VAR extents: Api.cairo_text_extents_t), NEW;
	BEGIN
		Api.cairo_text_extents (ct.cr, text, extents)
	END GetTextExtents;

	PROCEDURE (ct: Text) GetGlyphExtents* (glyphs: Api.cairo_glyph_t; num_glyphs: Api.Integer; VAR extents: Api.cairo_text_extents_t), NEW;
	BEGIN
		Api.cairo_glyph_extents (ct.cr, glyphs, num_glyphs, extents)
	END GetGlyphExtents;

	PROCEDURE (ct: Text) GetFontExtents* (VAR extents: Api.cairo_font_extents_t), NEW;
	BEGIN
		Api.cairo_font_extents (ct.cr, extents)
	END GetFontExtents;
	
END CairoTexts.
