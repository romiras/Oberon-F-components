MODULE TestCairoTransformDemo;

	IMPORT Api := LibsCairo, L := Out;

	PROCEDURE draw_shape* (VAR cr: Api.cairo_t);
	BEGIN
		Api.cairo_set_source_rgb (cr, 0, 0, 0);
		Api.cairo_move_to (cr, 0, 0);
		Api.cairo_line_to (cr, 100, 100);
		Api.cairo_move_to (cr, 100, 0);
		Api.cairo_line_to (cr, 0, 100);

		Api.cairo_save (cr);
		Api.cairo_identity_matrix (cr);
		Api.cairo_set_line_width (cr, 10);
		Api.cairo_stroke (cr);
		Api.cairo_restore (cr);
	END draw_shape;

	PROCEDURE TransformDemo*;
		VAR
			surface: Api.cairo_surface_t;
			cr: Api.cairo_t;
			mat: Api.cairo_matrix_t;
			status: Api.cairo_status_t;
	BEGIN
		surface := Api.cairo_image_surface_create(Api.CAIRO_FORMAT_ARGB32, 120, 120);
		cr := Api.cairo_create(surface);
		
		Api.cairo_set_source_rgb (cr, 0, 0, 0);
		Api.cairo_rectangle (cr, 1, 1, 120, 120);
		Api.cairo_set_source_rgb (cr, 1.0, 0, 0);
		Api.cairo_fill(cr);
		
		(*
		Api.cairo_set_source_rgb (cr, 0, 0, 0);
		Api.cairo_save (cr);
		draw_shape(cr);
		Api.cairo_restore (cr);
		
		Api.cairo_save (cr);
		Api.cairo_matrix_init (mat,
			1, 0.5,
			0, 1,
			50, 20);
		Api.cairo_transform (cr, mat);
		draw_shape(cr);
		Api.cairo_restore (cr);
		*)
		status := Api.cairo_surface_write_to_png(surface, "image_transform.png");
		L.Int(status, 0); L.Ln;
		
		Api.cairo_destroy(cr);
		Api.cairo_surface_destroy(surface);
	END TransformDemo;

END TestCairoTransformDemo.

TestCairoTransformDemo.TransformDemo
