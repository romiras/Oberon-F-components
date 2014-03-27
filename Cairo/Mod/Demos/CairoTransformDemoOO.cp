MODULE CairoTestTransform;

	IMPORT
		M := CairoModels, S := CairoSurfaces, Out;

	PROCEDURE DrawObjects (VAR ctx: M.Context);
		VAR mat: M.Matrix;

		PROCEDURE DrawShape (ctx: M.Context);
			VAR path: M.Path;
		BEGIN
			ctx.SetSourceRGB (0, 0, 0);
			path.ConnectTo(ctx);
			path.MoveTo (0, 0);
			path.LineTo (100, 100);
			path.MoveTo (100, 0);
			path.LineTo (0, 100);
			path.Rectangle(0, 0, 70, 50);
			
			ctx.Save;
			ctx.SetIdentityMatrix;
			ctx.SetLineWidth (10);
			ctx.Stroke;
			ctx.Restore
		END DrawShape;
		
	BEGIN
		ctx.SetSourceRGB (0, 0, 0);
		ctx.Save;
		DrawShape(ctx);
		ctx.Restore;

		mat.InitIdentity;
		mat.Rotate(1/12);
		
		(*
		mat.Translate (10, 10); mat.Scale (100, 100);
		mat.Scale (100, 100); (*mat.Translate (0.1, 0.1);*)
		*)
		ctx.TransformCTM (mat);
		DrawShape(ctx);
		
		ctx.Save;
		mat.Init(
			1, 0.5,
			0, 1,
			50, 20
		);
		ctx.TransformCTM (mat);
		DrawShape(ctx);
		ctx.Restore;
	END DrawObjects;
	
	PROCEDURE ExportAsImage (surface: M.Surface; IN fName: ARRAY OF SHORTCHAR; VAR status: M.Status);
	BEGIN
		status := surface.WriteToFile(fName)
	END ExportAsImage;
	
	PROCEDURE TestCairoTransform (VAR surface: M.MemBufSurface; VAR context: M.Context);
	VAR
		status: M.Status;
		fileName: ARRAY 1024 OF SHORTCHAR;
	BEGIN
		(* Draw objects on context *)
		DrawObjects(context);

		fileName := "image1.png";
		ExportAsImage(surface, fileName, status);
		IF status = M.STATUS_SUCCESS THEN
			Out.String("Image has been exported successfully."); Out.Ln
		END;
	END TestCairoTransform;
	
	PROCEDURE Do*;
		CONST NoMemError = "Failed to allocate memory buffer.";
		VAR memSurface: M.MemBufSurface; context: M.Context;
	BEGIN
		(* Create target surface (in memory buffer) *)
		NEW (memSurface);
		IF memSurface # NIL THEN
			memSurface.Create (M.FORMAT_ARGB32, 120, 120);
			
			(* Create context for given surface *)
			NEW (context);
			IF context # NIL THEN
				context.Create (memSurface);
				TestCairoTransform (memSurface, context);
				context.Destroy
			ELSE
				Out.String(NoMemError); Out.Ln
			END;
			memSurface.Destroy
		ELSE
			Out.String(NoMemError); Out.Ln
		END;
	END Do;

END CairoTestTransform.

CairoTestTransform.Do


