MODULE Bitmaps;
	
	IMPORT Ports, Stores, Models, Views;
	
	TYPE
		RootContext = POINTER TO RECORD (Models.Context)
			w, h: INTEGER
		END;
		
		RootView = POINTER TO RECORD (Views.View)
			view: Views.View;
		END;
	
	(* helpers for painting to bitmap *)	
	
	PROCEDURE (c: RootContext) ThisModel (): Models.Model;
	BEGIN
		RETURN NIL
	END ThisModel;

	PROCEDURE (c: RootContext) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := c.w; h := c.h
	END GetSize;
		
	PROCEDURE (c: RootContext) Normalize (): BOOLEAN;
	BEGIN
		RETURN TRUE
	END Normalize;
	
	PROCEDURE (d: RootView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		Views.InstallFrame(f, d.view, 0, 0, 0, FALSE)
	END Restore;
	
	PROCEDURE (d: RootView) GetNewFrame (VAR frame: Views.Frame);
		VAR f: Views.RootFrame;
	BEGIN
		NEW(f); frame := f
	END GetNewFrame;

	PROCEDURE (d: RootView) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;
	
	PROCEDURE Paint* (v: Views.View; p: Ports.Port; w, h, unit: INTEGER);
		VAR d: RootView; c: RootContext; f: Views.RootFrame; g: Views.Frame;
	BEGIN
		ASSERT(v # NIL); ASSERT(p # NIL); ASSERT((w > 0) & (h > 0));
		NEW(c);
		c.w := w * p.unit;
		c.h := h * p.unit;
		NEW(d);
		d.view := Views.CopyOf(v, Views.shallow);
		Stores.Join(d, d.view);
		d.InitContext(c);
		d.view.InitContext(c);
		Stores.InitDomain(d);
		d.GetNewFrame(g); f := g(Views.RootFrame); f.ConnectTo(p);
		Views.SetRoot(f, d, FALSE, {});
		Views.AdaptRoot(f);
		Views.RestoreRoot(f, 0, 0, c.w, c.h);
	END Paint;

END Bitmaps.