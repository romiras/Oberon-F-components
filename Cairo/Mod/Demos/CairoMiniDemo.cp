MODULE CairoTestMini;

	IMPORT Api := LibsCairo, L := Out;

	PROCEDURE RunDemo*;
		VAR
			surface: Api.cairo_surface_t;
			cr: Api.cairo_t;
			status: Api.cairo_status_t;
	BEGIN
		surface := Api.cairo_image_surface_create(Api.CAIRO_FORMAT_ARGB32, 120, 120);
		cr := Api.cairo_create(surface);
		
		Api.cairo_set_source_rgb (cr, 0.8, 0.8, 0);
		Api.cairo_rectangle (cr, 1, 1, 120, 120);
		Api.cairo_fill(cr);
		
		Api.cairo_set_line_width (cr, 1);
		Api.cairo_set_source_rgb (cr, 0, 0, 0);
		Api.cairo_rectangle (cr, 10, 10, 100, 100);
		Api.cairo_stroke (cr);
		
		status := Api.cairo_surface_write_to_png(surface, "image-mini.png");
		
		Api.cairo_destroy(cr);
		Api.cairo_surface_destroy(surface);
	END RunDemo;

END CairoTestMini.

CairoTestMini.RunDemo
