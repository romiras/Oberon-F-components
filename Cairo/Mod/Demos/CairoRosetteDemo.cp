MODULE CairoRosetteDemo;

	IMPORT Api := LibsCairo, Math, Out;

	PROCEDURE draw_shape (cr: Api.cairo_t);
	CONST
		xx = 600;
		yy = 500;
		R = 150.0;
		RR = 360.0;
		RR1 = 30.0;
		iStep = 2.0*3.1415926/180.0;
		jStep = 0.8*3.1415926/180.0;
	VAR
		x, y, z,
		X1, Y1,
		X2, Y2,
		s, t,
		P, rr0,
		i, j,
		red, green, blue: REAL;
	BEGIN
		X2 := 0;
		Y2 := 0;
		P := Math.Pi();
		X1 := X2;
		Y1 := Y2;

		Api.cairo_set_source_rgb (cr, 0, 0, 1);

		i := 0;
		REPEAT
			j := 0;
			REPEAT
				t := Math.Cos(2.0 * i);
				s := Math.Sin(1.0 * j + 4.0 * i);
				rr0 := RR1 + R * Math.Power (ABS(t) , 14) * Math.Sign(t) + R * Math.Power (ABS(s) , 24) * Math.Sign(s);

				x := RR * Math.Cos(i) + rr0 * Math.Cos(j);
				y := RR * Math.Sin(i) + rr0 * Math.Sin(j);
				z := 0; (*RR * Math.Sin(1 * i) + rr0 * Math.Cos(j)*)

				X1 := x / 1.0 - z * Math.Cos(P / 3) * 0.5;
				Y1 := y / 1.0 - z * Math.Sin(P / 3) * 0.5;

				red := rr0 / (RR1 + R);
				green := ABS(Math.Sin(i));
				blue := ABS(Math.Sin((j)));
				
				(*red := 255 * rr0 / (RR1 + 70);
				green := 255 * ABS(Math.Sin(i / 15));
				blue := 255 * ABS(Math.Sin((j) / 20));*)

				IF (i > 0) & (j > 0) THEN
					Api.cairo_set_source_rgb (cr, red, green, blue);
					Api.cairo_move_to (cr, X1 + xx, Y1 + yy);
					Api.cairo_line_to (cr, X2 + xx, Y2 + yy);
					(*L.Real(X1); L.Ln;*)
				END;

				X2 := X1;
				Y2 := Y1;
				j := j + jStep;
			UNTIL j > 2*P;

			i := i + iStep;
		UNTIL i > 2*P;
		Api.cairo_stroke(cr);
	END draw_shape;

	PROCEDURE RosetteDemo*;
		VAR
			surface: Api.cairo_surface_t;
			cr: Api.cairo_t;
			status: Api.cairo_status_t;
	BEGIN
		surface := Api.cairo_image_surface_create(Api.CAIRO_FORMAT_ARGB32, 1200, 1200);
		cr := Api.cairo_create(surface);

		draw_shape(cr);

		status := Api.cairo_surface_write_to_png(surface, "image_rosette.png");
		IF status # 0 THEN Out.Int(status, 0); Out.Ln END;

		Api.cairo_destroy(cr);
		Api.cairo_surface_destroy(surface);
	END RosetteDemo;

END CairoRosetteDemo.

CairoRosetteDemo.RosetteDemo
