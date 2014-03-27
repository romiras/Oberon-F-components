MODULE CairoTextDemo;

	IMPORT Api := LibsCairo, Out;

	PROCEDURE RunDemo*;
		VAR
			surface: Api.cairo_surface_t;
			cr: Api.cairo_t;
			status: Api.cairo_status_t;
	BEGIN
		surface := Api.cairo_image_surface_create(Api.CAIRO_FORMAT_ARGB32, 390, 60);
		cr := Api.cairo_create(surface);

		Api.cairo_set_source_rgb(cr, 0, 0, 0);
		Api.cairo_select_font_face(cr, 'Sans', Api.CAIRO_FONT_SLANT_NORMAL,
		Api.CAIRO_FONT_WEIGHT_NORMAL);
		Api.cairo_set_font_size(cr, 40.0);

		Api.cairo_move_to(cr, 10.0, 50.0);
		Api.cairo_show_text(cr, "Cairo text demo.");

		status := Api.cairo_surface_write_to_png(surface, "image.png");
		Out.Int(status, 0); Out.Ln;

		Api.cairo_destroy(cr);
		Api.cairo_surface_destroy(surface)
	END RunDemo;

END CairoTextDemo.

CairoTextDemo.RunDemo

