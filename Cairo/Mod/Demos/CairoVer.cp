MODULE CairoTestVer;

	IMPORT
		Api := LibsCairo, Out;

	PROCEDURE Do*;
		VAR ver: Api.String;
	BEGIN
		ver := Api.cairo_version_string();
		Out.String ("Cairo version: " + ver$); Out.Ln
	END Do;

END CairoTestVer.

CairoTestVer.Do

