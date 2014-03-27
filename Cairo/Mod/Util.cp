MODULE CairoUtil;


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

	(*Helper procedure to retrieve decoded version *)
	PROCEDURE CairoVersionDecode* (OUT major, minor, micro: Lib.Integer);
	VAR
		version: Lib.Integer;
	BEGIN
		version := Lib.cairo_version();
		major := version DIV 10000;
		minor := (version MOD (major * 10000)) DIV 100;
		micro := (version MOD ((major * 10000) + (minor * 100)));
	END CairoVersionDecode;

END CairoUtil.

