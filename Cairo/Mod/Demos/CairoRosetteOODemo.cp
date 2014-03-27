MODULE CairoRosetteOODemo;

	IMPORT M := CairoModels, S := CairoSurfaces, Math, Out;

	PROCEDURE DrawShape (ctx: M.Context);
		CONST
			xx = 600;
			yy = 500;
			R = 150.0;
			RR = 360.0;
			RR1 = 30.0;
			iStep = 2.0*3.1415926/180.0;
			jStep = 0.8*3.1415926/180.0;
		VAR
			path: M.Path;
			x, y, z,
			X1, Y1,
			X2, Y2,
			s, t,
			rr0,
			i, j,
			red, green, blue, pi, const2pi, c1, c2: REAL;
	BEGIN
		X2 := 0;
		Y2 := 0;
		pi := Math.Pi();
		const2pi := 2.0 * pi;
		c1 := Math.Cos(pi / 3.0) * 0.5; c2 := Math.Sin(pi / 3.0) * 0.5;
		X1 := X2;
		Y1 := Y2;

		path.ConnectTo(ctx);
		ctx.SetSourceRGB (0, 0, 1);

		i := 0;
		REPEAT
			j := 0;
			REPEAT
				t := Math.Cos(2.0 * i);
				s := Math.Sin(1.0 * j + 4.0 * i);
				rr0 := RR1 + R * Math.Power (ABS(t) , 14) * Math.Sign(t) + R * Math.Power (ABS(s) , 24) * Math.Sign(s);

				x := RR * Math.Cos(i) + rr0 * Math.Cos(j);
				y := RR * Math.Sin(i) + rr0 * Math.Sin(j);
				z := 0;

				X1 := x / 1.0 - z * c1;
				Y1 := y / 1.0 - z * c2;

				red := rr0 / (RR1 + R);
				green := ABS(Math.Sin(i));
				blue := ABS(Math.Sin((j)));
				
				(*red := 255 * rr0 / (RR1 + 70);
				green := 255 * ABS(Math.Sin(i / 15));
				blue := 255 * ABS(Math.Sin((j) / 20));*)

				IF (i > 0) & (j > 0) THEN
					ctx.SetSourceRGB (red, green, blue);
					path.MoveTo (X1 + xx, Y1 + yy);
					path.LineTo (X2 + xx, Y2 + yy)
				END;

				X2 := X1;
				Y2 := Y1;
				j := j +  jStep
			UNTIL j > const2pi;

			i := i + iStep
		UNTIL i > const2pi;
		ctx.Stroke;
	END DrawShape;

	PROCEDURE RosetteDemo*;
	VAR
		surface: M.MemBufSurface;
		context: M.Context;
		status: M.Status;
	BEGIN
		NEW (surface); surface.Create(M.FORMAT_ARGB32, 1200, 1200);
		NEW (context); context.Create(surface);

		DrawShape(context);

		status := surface.WriteToFile("image_rosette.png");
		IF status = M.STATUS_SUCCESS THEN
			Out.String("Done."); Out.Ln
		ELSE
			Out.String("Error #"); Out.Int(status, 0); Out.Ln
		END;

		context.Destroy;
		surface.Destroy
	END RosetteDemo;

END CairoRosetteOODemo.

CairoRosetteOODemo.RosetteDemo
