MODULE CairoModels;


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

	IMPORT Lib := LibsCairo;
	
	TYPE
    	Enum* = Lib.Enum;
		Integer* = Lib.Integer;
        LongWord* = Lib.LongWord;
		Real* = Lib.Real;
        SString* = Lib.String;
        ByteArr* = Lib.ByteArrPtr;
		RealArr* = Lib.RealArrPtr;
		Data* = Lib.Pointer;
		
		Status* = Lib.cairo_status_t;
		CrContext* = Lib.cairo_t;
		CrPath* = Lib.cairo_path_t;
		CrPathPtr* = POINTER TO CrPath;
		CrSurface* = Lib.cairo_surface_t;
		CrPattern* = Lib.cairo_pattern_t;
		CrFormat* = Lib.cairo_format_t;
		CrContent* = Lib.cairo_content_t;
		CrOperator* = Lib.cairo_operator_t;
		CrMatrix* = Lib.cairo_matrix_t;
		CrFilter* = Lib.cairo_filter_t;
		CrExtend* = Lib.cairo_extend_t;
		CrFillRule* = Lib.cairo_fill_rule_t;
		CrLineCap* = Lib.cairo_line_cap_t;
		CrLineJoin* = Lib.cairo_line_join_t;
		CrAntialias* = Lib.cairo_antialias_t;
		
		CrWriteProc* = Lib.cairo_write_func_t;
		CrDestroyProc* = Lib.cairo_destroy_func_t;
		CrUserDataKey* = Lib.cairo_user_data_key_t;
		
		Base* = POINTER TO ABSTRACT RECORD END;
		
		Context* = POINTER TO RECORD (Base)
			cr-: CrContext
		END;
		
		Path* = RECORD
			cr: CrContext
		END;
		
		Matrix* = RECORD
			m-: CrMatrix
		END;
		
		Pattern* = POINTER TO ABSTRACT RECORD (Base)
		    pattern-: CrPattern
		END;
		
		GradientPattern* = POINTER TO ABSTRACT RECORD (Pattern) END;

		Surface* = POINTER TO ABSTRACT RECORD (Base)
			surface-: CrSurface
		END;
		
		DocumentSurface* = POINTER TO EXTENSIBLE RECORD (Surface) END;
		MemBufSurface* = POINTER TO EXTENSIBLE RECORD (Surface) END;
		DrawableSurface* = POINTER TO EXTENSIBLE RECORD (Surface) END;
	
	CONST	
		STATUS_SUCCESS* = Lib.CAIRO_STATUS_SUCCESS;
		STATUS_NO_MEMORY* = Lib.CAIRO_STATUS_NO_MEMORY;
		STATUS_INVALID_RESTORE* = Lib.CAIRO_STATUS_INVALID_RESTORE;
		STATUS_INVALID_POP_GROUP* = Lib.CAIRO_STATUS_INVALID_POP_GROUP;
		STATUS_NO_CURRENT_POINT* = Lib.CAIRO_STATUS_NO_CURRENT_POINT;
		STATUS_INVALID_MATRIX* = Lib.CAIRO_STATUS_INVALID_MATRIX;
		STATUS_INVALID_STATUS* = Lib.CAIRO_STATUS_INVALID_STATUS;
		STATUS_NULL_POINTER* = Lib.CAIRO_STATUS_NULL_POINTER;
		STATUS_INVALID_STRING* = Lib.CAIRO_STATUS_INVALID_STRING;
		STATUS_INVALID_PATH_DATA* = Lib.CAIRO_STATUS_INVALID_PATH_DATA;
		STATUS_READ_ERROR* = Lib.CAIRO_STATUS_READ_ERROR;
		STATUS_WRITE_ERROR* = Lib.CAIRO_STATUS_WRITE_ERROR;
		STATUS_SURFACE_FINISHED* = Lib.CAIRO_STATUS_SURFACE_FINISHED;
		STATUS_SURFACE_TYPE_MISMATCH* = Lib.CAIRO_STATUS_SURFACE_TYPE_MISMATCH;
		STATUS_PATTERN_TYPE_MISMATCH* = Lib.CAIRO_STATUS_PATTERN_TYPE_MISMATCH;
		STATUS_INVALID_CONTENT* = Lib.CAIRO_STATUS_INVALID_CONTENT;
		STATUS_INVALID_FORMAT* = Lib.CAIRO_STATUS_INVALID_FORMAT;
		STATUS_INVALID_VISUAL* = Lib.CAIRO_STATUS_INVALID_VISUAL;
		STATUS_FILE_NOT_FOUND* = Lib.CAIRO_STATUS_FILE_NOT_FOUND;
		STATUS_INVALID_DASH* = Lib.CAIRO_STATUS_INVALID_DASH;

		OPERATOR_CLEAR* = Lib.CAIRO_OPERATOR_CLEAR;
		OPERATOR_SOURCE* = Lib.CAIRO_OPERATOR_SOURCE;
		OPERATOR_OVER* = Lib.CAIRO_OPERATOR_OVER;
		OPERATOR_IN* = Lib.CAIRO_OPERATOR_IN;
		OPERATOR_OUT* = Lib.CAIRO_OPERATOR_OUT;
		OPERATOR_ATOP* = Lib.CAIRO_OPERATOR_ATOP;
		OPERATOR_DEST* = Lib.CAIRO_OPERATOR_DEST;
		OPERATOR_DEST_OVER* = Lib.CAIRO_OPERATOR_DEST_OVER;
		OPERATOR_DEST_IN* = Lib.CAIRO_OPERATOR_DEST_IN;
		OPERATOR_DEST_OUT* = Lib.CAIRO_OPERATOR_DEST_OUT;
		OPERATOR_DEST_ATOP* = Lib.CAIRO_OPERATOR_DEST_ATOP;
		OPERATOR_XOR* = Lib.CAIRO_OPERATOR_XOR;
		OPERATOR_ADD* = Lib.CAIRO_OPERATOR_ADD;
		OPERATOR_SATURATE* = Lib.CAIRO_OPERATOR_SATURATE;

		ANTIALIAS_DEFAULT* = Lib.CAIRO_ANTIALIAS_DEFAULT;
		ANTIALIAS_NONE* = Lib.CAIRO_ANTIALIAS_NONE;
		ANTIALIAS_GRAY* = Lib.CAIRO_ANTIALIAS_GRAY;
		ANTIALIAS_SUBPIXEL* = Lib.CAIRO_ANTIALIAS_SUBPIXEL;

		FILL_RULE_WINDING* = Lib.CAIRO_FILL_RULE_WINDING;
		FILL_RULE_EVEN_ODD* = Lib.CAIRO_FILL_RULE_EVEN_ODD;

		LINE_CAP_BUTT* = Lib.CAIRO_LINE_CAP_BUTT;
		LINE_CAP_ROUND* = Lib.CAIRO_LINE_CAP_ROUND;
		LINE_CAP_SQUARE* = Lib.CAIRO_LINE_CAP_SQUARE;

		LINE_JOIN_MITER* = Lib.CAIRO_LINE_JOIN_MITER;
		LINE_JOIN_ROUND* = Lib.CAIRO_LINE_JOIN_ROUND;
		LINE_JOIN_BEVEL* = Lib.CAIRO_LINE_JOIN_BEVEL;

		FONT_SLANT_NORMAL* = Lib.CAIRO_FONT_SLANT_NORMAL;
		FONT_SLANT_ITALIC* = Lib.CAIRO_FONT_SLANT_ITALIC;
		FONT_SLANT_OBLIQUE* = Lib.CAIRO_FONT_SLANT_OBLIQUE;

		FONT_WEIGHT_NORMAL* = Lib.CAIRO_FONT_WEIGHT_NORMAL;
		FONT_WEIGHT_BOLD* = Lib.CAIRO_FONT_WEIGHT_BOLD;

		SUBPIXEL_ORDER_DEFAULT* = Lib.CAIRO_SUBPIXEL_ORDER_DEFAULT;
		SUBPIXEL_ORDER_RGB* = Lib.CAIRO_SUBPIXEL_ORDER_RGB;
		SUBPIXEL_ORDER_BGR* = Lib.CAIRO_SUBPIXEL_ORDER_BGR;
		SUBPIXEL_ORDER_VRGB* = Lib.CAIRO_SUBPIXEL_ORDER_VRGB;
		SUBPIXEL_ORDER_VBGR* = Lib.CAIRO_SUBPIXEL_ORDER_VBGR;

		HINT_STYLE_DEFAULT* = Lib.CAIRO_HINT_STYLE_DEFAULT;
		HINT_STYLE_NONE* = Lib.CAIRO_HINT_STYLE_NONE;
		HINT_STYLE_SLIGHT* = Lib.CAIRO_HINT_STYLE_SLIGHT;
		HINT_STYLE_MEDIUM* = Lib.CAIRO_HINT_STYLE_MEDIUM;
		HINT_STYLE_FULL* = Lib.CAIRO_HINT_STYLE_FULL;

		HINT_METRICS_DEFAULT* = Lib.CAIRO_HINT_METRICS_DEFAULT;
		HINT_METRICS_OFF* = Lib.CAIRO_HINT_METRICS_OFF;
		HINT_METRICS_ON* = Lib.CAIRO_HINT_METRICS_ON;

		PATH_MOVE_TO* = Lib.CAIRO_PATH_MOVE_TO;
		PATH_LINE_TO* = Lib.CAIRO_PATH_LINE_TO;
		PATH_CURVE_TO* = Lib.CAIRO_PATH_CURVE_TO;
		PATH_CLOSE_PATH* = Lib.CAIRO_PATH_CLOSE_PATH;

		CONTENT_COLOR* = Lib.CAIRO_CONTENT_COLOR;
		CONTENT_ALPHA* = Lib.CAIRO_CONTENT_ALPHA;
		CONTENT_COLOR_ALPHA* = Lib.CAIRO_CONTENT_COLOR_ALPHA;

		FORMAT_ARGB32* = Lib.CAIRO_FORMAT_ARGB32;
		FORMAT_RGB24* = Lib.CAIRO_FORMAT_RGB24;
		FORMAT_A8* = Lib.CAIRO_FORMAT_A8;
		FORMAT_A1* = Lib.CAIRO_FORMAT_A1;

		EXTEND_NONE* = Lib.CAIRO_EXTEND_NONE;
		EXTEND_REPEAT* = Lib.CAIRO_EXTEND_REPEAT;
		EXTEND_REFLECT* = Lib.CAIRO_EXTEND_REFLECT;
		EXTEND_PAD* = Lib.CAIRO_EXTEND_PAD;

		FILTER_FAST* = Lib.CAIRO_FILTER_FAST;
		FILTER_GOOD* = Lib.CAIRO_FILTER_GOOD;
		FILTER_BEST* = Lib.CAIRO_FILTER_BEST;
		FILTER_NEAREST* = Lib.CAIRO_FILTER_NEAREST;
		FILTER_BILINEAR* = Lib.CAIRO_FILTER_BILINEAR;
		FILTER_GAUSSIAN* = Lib.CAIRO_FILTER_GAUSSIAN;

		FONT_TYPE_TOY* = Lib.CAIRO_FONT_TYPE_TOY;
		FONT_TYPE_FT* = Lib.CAIRO_FONT_TYPE_FT;
		FONT_TYPE_WIN32* = Lib.CAIRO_FONT_TYPE_WIN32;
		FONT_TYPE_ATSUI* = Lib.CAIRO_FONT_TYPE_ATSUI;

		PATTERN_TYPE_SOLID* = Lib.CAIRO_PATTERN_TYPE_SOLID;
		PATTERN_TYPE_SURFACE* = Lib.CAIRO_PATTERN_TYPE_SURFACE;
		PATTERN_TYPE_LINEAR* = Lib.CAIRO_PATTERN_TYPE_LINEAR;
		PATTERN_TYPE_RADIAL* = Lib.CAIRO_PATTERN_TYPE_RADIAL;

		SURFACE_TYPE_IMAGE* = Lib.CAIRO_SURFACE_TYPE_IMAGE;
		SURFACE_TYPE_PDF* = Lib.CAIRO_SURFACE_TYPE_PDF;
		SURFACE_TYPE_PS* = Lib.CAIRO_SURFACE_TYPE_PS;
		SURFACE_TYPE_XLIB* = Lib.CAIRO_SURFACE_TYPE_XLIB;
		SURFACE_TYPE_XCB* = Lib.CAIRO_SURFACE_TYPE_XCB;
		SURFACE_TYPE_GLITZ* = Lib.CAIRO_SURFACE_TYPE_GLITZ;
		SURFACE_TYPE_QUARTZ* = Lib.CAIRO_SURFACE_TYPE_QUARTZ;
		SURFACE_TYPE_WIN32* = Lib.CAIRO_SURFACE_TYPE_WIN32;
		SURFACE_TYPE_BEOS* = Lib.CAIRO_SURFACE_TYPE_BEOS;
		SURFACE_TYPE_DIRECTFB* = Lib.CAIRO_SURFACE_TYPE_DIRECTFB;
		SURFACE_TYPE_SVG* = Lib.CAIRO_SURFACE_TYPE_SVG;
		SURFACE_TYPE_OS2* = Lib.CAIRO_SURFACE_TYPE_OS2;

		SVG_VERSION_1_1* = Lib.CAIRO_SVG_VERSION_1_1;
		SVG_VERSION_1_2* = Lib.CAIRO_SVG_VERSION_1_2;
		
	
	(* Base *)
	
	(*PROCEDURE (b: Base) GetReferenceCount*(): LongWord, NEW, ABSTRACT;*)
	PROCEDURE (b: Base) Destroy*, NEW, ABSTRACT;
	PROCEDURE (b: Base) GetUserData* (key: CrUserDataKey): Data, NEW, ABSTRACT;
	PROCEDURE (b: Base ) SetUserData* (key: CrUserDataKey;
		user_data: Data; destroy_func: CrDestroyProc): Status, NEW, ABSTRACT;
	PROCEDURE (b: Base ) GetStatus* (): Status, NEW, ABSTRACT;


	(* Context *)
	
	PROCEDURE (C: Context) Create* (target: Surface), NEW;
	BEGIN
		C.cr := Lib.cairo_create (target.surface)
	END Create;
	
	(*
	PROCEDURE (C: Context) Reference*(): CrContext, NEW;
	BEGIN
		RETURN Lib.cairo_reference (C.cr)
	END Reference;

	PROCEDURE (C: Context) GetReferenceCount*(): LongWord, NEW;
	BEGIN
		RETURN Lib.cairo_get_reference_count (C.cr)
	END GetReferenceCount;
	*)
	
	PROCEDURE (C: Context) Destroy*;
	BEGIN
		Lib.cairo_destroy (C.cr)
	END Destroy;

	PROCEDURE (C: Context) GetStatus*(): Status;
	BEGIN
		RETURN Lib.cairo_status (C.cr)
	END GetStatus;

	PROCEDURE (C: Context) GetUserData* (key: CrUserDataKey): Data;
	BEGIN
		RETURN Lib.cairo_get_user_data (C.cr, key)
	END GetUserData;

	PROCEDURE (C: Context) SetUserData* (key: CrUserDataKey;
		user_data: Data; destroy_func: CrDestroyProc): Status;
	BEGIN
		RETURN Lib.cairo_set_user_data (C.cr, key, user_data, destroy_func)
	END SetUserData;

	PROCEDURE (C: Context) SetIdentityMatrix*, NEW;
	BEGIN
		Lib.cairo_identity_matrix (C.cr)
	END SetIdentityMatrix;

	PROCEDURE (C: Context) SetCTM* (matrix: CrMatrix), NEW;
	BEGIN
		Lib.cairo_set_matrix (C.cr, matrix)
	END SetCTM;

	PROCEDURE (C: Context) GetCTM* (VAR M: CrMatrix), NEW;
	BEGIN
		Lib.cairo_get_matrix (C.cr, M);
	END GetCTM;

	PROCEDURE (C: Context) TransformCTM* (matrix: Matrix), NEW;
	BEGIN
		Lib.cairo_transform (C.cr, matrix.m)
	END TransformCTM;

	PROCEDURE (C: Context) SetSource* (p: Pattern), NEW;
	BEGIN
		Lib.cairo_set_source (C.cr, p.pattern)
	END SetSource;

	PROCEDURE (C: Context) SetSourceSurface* (s: Surface; x, y: Real), NEW;
	BEGIN
		Lib.cairo_set_source_surface (C.cr, s.surface, x, y)
	END SetSourceSurface;

	PROCEDURE (C: Context) SetLineWidth* (width: Real), NEW;
	BEGIN
		Lib.cairo_set_line_width (C.cr, width)
	END SetLineWidth;

	PROCEDURE (C: Context) GetLineWidth* (): Real, NEW;
	BEGIN
		RETURN Lib.cairo_get_line_width (C.cr)
	END GetLineWidth;

	PROCEDURE (C: Context) SetLineCap* (line_cap: CrLineCap), NEW;
	BEGIN
		Lib.cairo_set_line_cap (C.cr, line_cap)
	END SetLineCap;

	PROCEDURE (C: Context) GetLineCap* (): CrLineCap, NEW;
	BEGIN
		RETURN Lib.cairo_get_line_cap (C.cr)
	END GetLineCap;

	PROCEDURE (C: Context) SetLineJoin* (line_join: CrLineJoin), NEW;
	BEGIN
		Lib.cairo_set_line_join (C.cr, line_join)
	END SetLineJoin;

	PROCEDURE (C: Context) GetLineJoin* (): CrLineJoin, NEW;
	BEGIN
		RETURN Lib.cairo_get_line_join (C.cr)
	END GetLineJoin;

	PROCEDURE (C: Context) SetMiterLimit* (limit: Real), NEW;
	BEGIN
		Lib.cairo_set_miter_limit (C.cr, limit)
	END SetMiterLimit;

	PROCEDURE (C: Context) GetMiterLimit* (): Real, NEW;
	BEGIN
		RETURN Lib.cairo_get_miter_limit (C.cr)
	END GetMiterLimit;

	PROCEDURE (C: Context) SetDash* (dashes: RealArr; num_dashes: Integer; offset: Real), NEW;
	BEGIN
		Lib.cairo_set_dash (C.cr, dashes, num_dashes, offset)
	END SetDash;

	PROCEDURE (C: Context) GetDashCount* (): Integer, NEW;
	BEGIN
		RETURN Lib.cairo_get_dash_count (C.cr)
	END GetDashCount;

	PROCEDURE (C: Context) GetDash* (VAR dashes: RealArr; VAR offset: Real), NEW;
	BEGIN
		Lib.cairo_get_dash (C.cr, dashes, offset)
	END GetDash;

	PROCEDURE (C: Context) SetFillRule* (fill_rule: CrFillRule), NEW;
	BEGIN
		Lib.cairo_set_fill_rule (C.cr, fill_rule)
	END SetFillRule;

	PROCEDURE (C: Context) GetFillRule* (): CrFillRule, NEW;
	BEGIN
		RETURN Lib.cairo_get_fill_rule (C.cr)
	END GetFillRule;

	PROCEDURE (C: Context) Paint*, NEW;
	BEGIN
		Lib.cairo_paint (C.cr)
	END Paint;

	PROCEDURE (C: Context) PaintWithAlpha* (alpha: Real), NEW;
	BEGIN
		Lib.cairo_paint_with_alpha (C.cr, alpha)
	END PaintWithAlpha;

	PROCEDURE (C: Context) Mask* (pattern: CrPattern), NEW;
	BEGIN
		Lib.cairo_mask (C.cr,pattern)
	END Mask;

	PROCEDURE (C: Context) MaskSurface* (surface: CrSurface; surface_x, surface_y: Real), NEW;
	BEGIN
		Lib.cairo_mask_surface (C.cr,surface, surface_x, surface_y)
	END MaskSurface;

	PROCEDURE (C: Context) Stroke*, NEW;
	BEGIN
		Lib.cairo_stroke (C.cr)
	END Stroke;

	PROCEDURE (C: Context) StrokePreserve*, NEW;
	BEGIN
		Lib.cairo_stroke_preserve (C.cr)
	END StrokePreserve;

	PROCEDURE (C: Context) Fill*, NEW;
	BEGIN
		Lib.cairo_fill (C.cr)
	END Fill;

	PROCEDURE (C: Context) FillPreserve*, NEW;
	BEGIN
		Lib.cairo_fill_preserve (C.cr)
	END FillPreserve;

	PROCEDURE (C: Context) Clip*, NEW;
	BEGIN
		Lib.cairo_clip (C.cr)
	END Clip;

	PROCEDURE (C: Context) ClipPreserve*, NEW;
	BEGIN
		Lib.cairo_clip_preserve (C.cr)
	END ClipPreserve;

	PROCEDURE (C: Context) ResetClip*, NEW;
	BEGIN
		Lib.cairo_reset_clip (C.cr)
	END ResetClip;

	PROCEDURE (C: Context) ClipExtents* (VAR x1, y1, x2, y2:  Real), NEW;
	BEGIN
		Lib.cairo_clip_extents (C.cr, x1, y1, x2, y2)
	END ClipExtents;

	PROCEDURE (C: Context) CopyClipRectangleList*(): Lib.cairo_rectangle_list_tPtr, NEW;
	BEGIN
		RETURN Lib.cairo_copy_clip_rectangle_list (C.cr)
	END CopyClipRectangleList;

	PROCEDURE (C: Context) RectangleListDestroy* (rectangle_list: Lib.cairo_rectangle_list_t), NEW;
	BEGIN
		Lib.cairo_rectangle_list_destroy (rectangle_list)
	END RectangleListDestroy;

	PROCEDURE (C: Context) Save*, NEW;
	BEGIN
		Lib.cairo_save (C.cr)
	END Save;

	PROCEDURE (C: Context) Restore*, NEW;
	BEGIN
		Lib.cairo_restore (C.cr)
	END Restore;

	PROCEDURE (C: Context) PushGroup*, NEW;
	BEGIN
		Lib.cairo_push_group (C.cr)
	END PushGroup;

	PROCEDURE (C: Context) PushGroupWithContent* (content:  CrContent), NEW;
	BEGIN
		Lib.cairo_push_group_with_content (C.cr, content)
	END PushGroupWithContent;

	PROCEDURE (C: Context) PopGroup*():  CrPattern, NEW;
	BEGIN
		RETURN Lib.cairo_pop_group (C.cr)
	END PopGroup;

	PROCEDURE (C: Context) PopGroupToSource*, NEW;
	BEGIN
		Lib.cairo_pop_group_to_source (C.cr)
	END PopGroupToSource;

	PROCEDURE (C: Context) SetSourceRGB*(red, green, blue: Real), NEW;
	BEGIN
		Lib.cairo_set_source_rgb (C.cr, red, green, blue)
	END SetSourceRGB;

	PROCEDURE (C: Context) SetSourceRGBA*(red, green, blue, alpha: Real), NEW;
	BEGIN
		Lib.cairo_set_source_rgba (C.cr, red, green, blue, alpha)
	END SetSourceRGBA;

	PROCEDURE (C: Context) SetOperator* (op: CrOperator), NEW;
	BEGIN
		Lib.cairo_set_operator (C.cr, op)
	END SetOperator;

	PROCEDURE (C: Context) GetOperator*  (): CrOperator, NEW;
	BEGIN
		RETURN Lib.cairo_get_operator (C.cr)
	END GetOperator;

	PROCEDURE (C: Context) CopyPage*, NEW;
	BEGIN
		Lib.cairo_copy_page (C.cr)
	END CopyPage;

	PROCEDURE (C: Context) ShowPage*, NEW;
	BEGIN
		Lib.cairo_show_page (C.cr)
	END ShowPage;

	PROCEDURE (C: Context) SetTolerance* (tolerance: Real), NEW;
	BEGIN
		Lib.cairo_set_tolerance (C.cr, tolerance)
	END SetTolerance;

	PROCEDURE (C: Context) GetTolerance* (): Real, NEW;
	BEGIN
		RETURN Lib.cairo_get_tolerance (C.cr)
	END GetTolerance;

	PROCEDURE (C: Context) GetAntialias*(): CrAntialias, NEW;
	BEGIN
		RETURN Lib.cairo_get_antialias (C.cr)
	END GetAntialias;

	PROCEDURE (C: Context) SetAntialias* (antialias: CrAntialias), NEW;
	BEGIN
		Lib.cairo_set_antialias (C.cr, antialias)
	END SetAntialias;


	(* Path *)

	PROCEDURE (VAR P: Path) ConnectTo* (ctx: Context), NEW;
	BEGIN
		ASSERT(ctx # NIL);
		P.cr := ctx.cr
	END ConnectTo;

	PROCEDURE (VAR P: Path) NewPath*, NEW;
	BEGIN
		Lib.cairo_new_path(P.cr)
	END NewPath;

	PROCEDURE (VAR P: Path) NewSubPath*, NEW;
	BEGIN
		Lib.cairo_new_sub_path(P.cr)
	END NewSubPath;

	PROCEDURE (VAR P: Path) ClosePath*, NEW;
	BEGIN
		Lib.cairo_close_path(P.cr)
	END ClosePath;

	PROCEDURE (VAR P: Path) AppendPath* (path: CrPath), NEW;
	BEGIN
		Lib.cairo_append_path(P.cr, path)
	END AppendPath;

	PROCEDURE (VAR P: Path) CopyPath* (): CrPathPtr, NEW;
	BEGIN
		RETURN Lib.cairo_copy_path (P.cr)
	END CopyPath;

	PROCEDURE (VAR P: Path) MoveTo* (x, y: Real), NEW;
	BEGIN
		Lib.cairo_move_to(P.cr,x,y)
	END MoveTo;

	PROCEDURE (VAR P: Path) MoveToRel (dx, dy: Real), NEW;
	BEGIN
		Lib.cairo_rel_move_to(P.cr, dx, dy)
	END MoveToRel;

	PROCEDURE (VAR P: Path) GetCurrentPoint* (VAR x, y: Real), NEW;
	BEGIN
		Lib.cairo_get_current_point(P.cr, x, y);
	END GetCurrentPoint;

	(*PROCEDURE (VAR P: Path) GetExtents (var x1, y1, x2, y2: Real);
	BEGIN
		Lib.cairo_path_extents(P.cr, x1, y1, x2, y2)
	END GetExtents;*)

	PROCEDURE (VAR P: Path) LineTo*(x, y: Real), NEW;
	BEGIN
		Lib.cairo_line_to(P.cr,x,y)
	END LineTo;

	PROCEDURE (VAR P: Path) LineToRel*(dx, dy: Real), NEW;
	BEGIN
		Lib.cairo_rel_move_to(P.cr,dx,dy)
	END LineToRel;

	PROCEDURE (VAR P: Path) Rectangle*(x, y, width, height: Real), NEW;
	BEGIN
		Lib.cairo_rectangle(P.cr,x, y, width, height)
	END Rectangle;

	PROCEDURE (VAR P: Path) CurveTo*(x1, y1, x2, y2, x3, y3: Real), NEW;
	BEGIN
		Lib.cairo_curve_to(P.cr,x1, y1, x2, y2, x3, y3)
	END CurveTo;

	PROCEDURE (VAR P: Path) CurveToRel(dx1, dy1, dx2, dy2, dx3, dy3: Real), NEW;
	BEGIN
	END CurveToRel;

	PROCEDURE (VAR P: Path) Arc*(xc, yc, radius, angle1, angle2: Real), NEW;
	BEGIN
		Lib.cairo_arc(P.cr,xc, yc, radius, angle1, angle2)
	END Arc;

	PROCEDURE (VAR P: Path) ArcNegative*(xc, yc, radius, angle1, angle2: Real), NEW;
	BEGIN
		Lib.cairo_arc_negative(P.cr,xc, yc, radius, angle1, angle2)
	END ArcNegative;


	(* Matrix *)
	
	PROCEDURE (VAR M: Matrix) Init* (xx, yx, xy, yy, x0, y0: Real), NEW;
	BEGIN
		Lib.cairo_matrix_init(M.m, xx, yx, xy, yy, x0, y0)
	END Init;
	
	PROCEDURE (VAR M: Matrix) InitIdentity*, NEW;
	BEGIN
		Lib.cairo_matrix_init_identity(M.m)
	END InitIdentity;
	
	PROCEDURE (VAR M: Matrix) Invert* (): Status, NEW;
	BEGIN
		 RETURN Lib.cairo_matrix_invert(M.m)
	END Invert;
	
	PROCEDURE (VAR M: Matrix) Translate* (tx, ty: Real), NEW;
	BEGIN
		Lib.cairo_matrix_translate(M.m, tx, ty)
	END Translate;
	
	PROCEDURE (VAR M: Matrix) Scale* (sx, sy: Real), NEW;
	BEGIN
		Lib.cairo_matrix_scale(M.m, sx, sy)
	END Scale;
	
	PROCEDURE (VAR M: Matrix) Rotate* (radians: Real), NEW;
	BEGIN
		Lib.cairo_matrix_rotate(M.m, radians)
	END Rotate;

	(* GradientPattern *)

	PROCEDURE (gp: GradientPattern) AddColorStopRGB* (offset, red, green, blue: Real), NEW;
	BEGIN
		Lib.cairo_pattern_add_color_stop_rgb (gp.pattern, offset, red, green, blue)
	END AddColorStopRGB;

	PROCEDURE (gp: GradientPattern) AddColorStopRGBA* (offset, red, green, blue, alpha: Real), NEW;
	BEGIN
		Lib.cairo_pattern_add_color_stop_rgba (gp.pattern, offset, red, green, blue, alpha)
	END AddColorStopRGBA;

	PROCEDURE (gp: GradientPattern) GetColorStopCount* (VAR count: Integer): Status, NEW;
	BEGIN
		RETURN Lib.cairo_pattern_get_color_stop_count (gp.pattern, count)
	END GetColorStopCount;


	(* Surface *)

	PROCEDURE (S: Surface) Assign* (surface: CrSurface), NEW;
	BEGIN
		ASSERT (surface # NIL);
		S.surface := surface
	END Assign;

	PROCEDURE (S: Surface) Destroy*;
	BEGIN
		Lib.cairo_surface_destroy (S.surface)
	END Destroy;

	PROCEDURE (S: Surface) GetStatus* (): Status;
	BEGIN
		RETURN Lib.cairo_surface_status (S.surface)
	END GetStatus;

	(*
	PROCEDURE (S: Surface) Reference* (): CrSurface, NEW;
	BEGIN
		RETURN Lib.cairo_surface_reference (S.surface)
	END Reference;

	PROCEDURE (S: Surface) GetReferenceCount*(): LongWord;
	BEGIN
		RETURN Lib.cairo_surface_get_reference_count (S.surface)
	END GetReferenceCount;
	*)

	PROCEDURE (S: Surface) GetType* (): Lib.cairo_surface_type_t, NEW;
	BEGIN
		RETURN Lib.cairo_surface_get_type (S.surface)
	END GetType;

	PROCEDURE (S: Surface) GetContent* (): CrContent, NEW;
	BEGIN
		RETURN Lib.cairo_surface_get_content (S.surface)
	END GetContent;

	PROCEDURE (S: Surface) GetUserData* (key: CrUserDataKey): Data;
	BEGIN
		RETURN Lib.cairo_surface_get_user_data (S.surface, key)
	END GetUserData;

	PROCEDURE (S: Surface) SetUserData* (key: CrUserDataKey;
		user_data: Data; destroy_func: CrDestroyProc): Status;
	BEGIN
		RETURN Lib.cairo_surface_set_user_data (S.surface, key, user_data, destroy_func)
	END SetUserData;

	PROCEDURE (S: Surface) Finish*, NEW;
	BEGIN
		Lib.cairo_surface_finish (S.surface)
	END Finish;

	PROCEDURE (S: Surface) Flush*, NEW;
	BEGIN
		Lib.cairo_surface_flush (S.surface)
	END Flush;

	PROCEDURE (S: Surface) MarkDirty*, NEW;
	BEGIN
		Lib.cairo_surface_mark_dirty (S.surface)
	END MarkDirty;

	PROCEDURE (S: Surface) MarkDirtyRectangle* (x, y, width, height: Integer), NEW;
	BEGIN
		Lib.cairo_surface_mark_dirty_rectangle (S.surface, x, y, width, height)
	END MarkDirtyRectangle;

	PROCEDURE (S: Surface) SetDeviceOffset* (x_offset, y_offset: Real), NEW;
	BEGIN
		Lib.cairo_surface_set_device_offset (S.surface, x_offset, y_offset)
	END SetDeviceOffset;

	PROCEDURE (S: Surface) GetDeviceOffset* (VAR x_offset, y_offset: Real), NEW;
	BEGIN
		Lib.cairo_surface_get_device_offset (S.surface, x_offset, y_offset)
	END GetDeviceOffset;

	PROCEDURE (S: Surface) WriteToFile* (Afilename: SString): Status, NEW;
	BEGIN
		RETURN Lib.cairo_surface_write_to_png (S.surface, Afilename)
	END WriteToFile;


	(* Pattern *)

	PROCEDURE (P: Pattern) Create* (Apattern: CrPattern),  NEW;
	BEGIN
		P.pattern := Apattern
	END Create;

	PROCEDURE (P: Pattern) Destroy*;
	BEGIN
		Lib.cairo_pattern_destroy (P.pattern)
	END Destroy;

	PROCEDURE (P: Pattern) GetStatus* (): Status;
	BEGIN
		RETURN Lib.cairo_pattern_status (P.pattern)
	END GetStatus;

	(*
	PROCEDURE (P: Pattern) Reference* (): CrPattern,  NEW;
	BEGIN
		RETURN  Lib.cairo_pattern_reference (P.pattern)
	END Reference;

	PROCEDURE (P: Pattern) GetReferenceCount*(): LongWord;
	BEGIN
		RETURN  Lib.cairo_pattern_get_reference_count (P.pattern)
	END GetReferenceCount;
	*)

	PROCEDURE (P: Pattern) GetUserData* (key: CrUserDataKey): Data;
	BEGIN
		RETURN  Lib.cairo_pattern_get_user_data (P.pattern, key)
	END GetUserData;

	PROCEDURE (P: Pattern) SetUserData* (key: CrUserDataKey;
		user_data: Data; destroy_func: CrDestroyProc): Status;
	BEGIN
		RETURN  Lib.cairo_pattern_set_user_data (P.pattern, key, user_data, destroy_func)
	END SetUserData;

	PROCEDURE (P: Pattern) SetFilter* (filter: CrFilter),  NEW;
	BEGIN
		Lib.cairo_pattern_set_filter (P.pattern, filter)
	END SetFilter;

	PROCEDURE (P: Pattern) GetFilter* (): CrFilter,  NEW;
	BEGIN
		RETURN  Lib.cairo_pattern_get_filter (P.pattern)
	END GetFilter;

	PROCEDURE (P: Pattern) SetMatrix* (matrix: CrMatrix),  NEW;
	BEGIN
		Lib.cairo_pattern_set_matrix (P.pattern, matrix)
	END SetMatrix;

	PROCEDURE (P: Pattern) GetMatrix* (VAR matrix: CrMatrix),  NEW;
	BEGIN
		Lib.cairo_pattern_get_matrix (P.pattern, matrix)
	END GetMatrix;

	PROCEDURE (P: Pattern) SetExtend* (extend: CrExtend),  NEW;
	BEGIN
		Lib.cairo_pattern_set_extend (P.pattern, extend)
	END SetExtend;

	PROCEDURE (P: Pattern) GetExtend*(): CrExtend,  NEW;
	BEGIN
		RETURN  Lib.cairo_pattern_get_extend (P.pattern)
	END GetExtend;


	(* MemBufSurface *)

	PROCEDURE (ims: MemBufSurface) Create* (Aformat: CrFormat; Awidth, Aheight: Integer), NEW;
	BEGIN
		ims.surface := Lib.cairo_image_surface_create (Aformat, Awidth, Aheight)
	END Create;

	PROCEDURE (ims: MemBufSurface) GetFormat* (): CrFormat, NEW;
	BEGIN
		RETURN Lib.cairo_image_surface_get_format (ims.surface)
	END GetFormat;

	PROCEDURE (ims: MemBufSurface) GetWidth* (): Integer, NEW;
	BEGIN
		RETURN Lib.cairo_image_surface_get_width (ims.surface)
	END GetWidth;

	PROCEDURE (ims: MemBufSurface) GetHeight* (): Integer, NEW;
	BEGIN
		RETURN Lib.cairo_image_surface_get_height (ims.surface)
	END GetHeight;

	PROCEDURE (ims: MemBufSurface) GetStride* (): Integer, NEW;
	BEGIN
		RETURN Lib.cairo_image_surface_get_stride (ims.surface)
	END GetStride;

	PROCEDURE (ims: MemBufSurface) StreamWrite*  (write_func: CrWriteProc; closure: Data): Status, NEW;
	BEGIN
		RETURN Lib.cairo_surface_write_to_png_stream (ims.surface, write_func, closure)
	END StreamWrite;


	(* DrawableSurface *)

	(*PROCEDURE (rs: DrawableSurface) SetSize* (Awidth, Aheight: Integer), NEW, EMPTY;*)
	(*PROCEDURE (rs: DrawableSurface) StreamWrite* (write_func: CrWriteProc;
		closure: Data; Awidth, Aheight: Integer): CrSurface, NEW, ABSTRACT;*)


	(* DocumentSurface *)

	PROCEDURE (ds: DocumentSurface) SetSize* (Awidth, Aheight: Real), NEW, EMPTY;

	PROCEDURE (ds: DocumentSurface) SetFallbackResolution* (x_pixels_per_inch, y_pixels_per_inch: Real), NEW;
	BEGIN
		Lib.cairo_surface_set_fallback_resolution (ds.surface, x_pixels_per_inch, y_pixels_per_inch);
	END SetFallbackResolution;

	(*PROCEDURE (ds: DocumentSurface) StreamRead* (read_func: Lib.cairo_read_func_t;
		closure: Data): CrSurface;
	PROCEDURE (ds: DocumentSurface) StreamWrite* (write_func: CrWriteProc;
		closure: Data; width_in_points, height_in_points: Real): CrSurface;*)


	PROCEDURE NewContext* (): Context;
		VAR ctx: Context;
	BEGIN
		NEW(ctx);
		RETURN ctx
	END NewContext;

END CairoModels.
