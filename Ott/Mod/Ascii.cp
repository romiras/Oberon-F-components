(*	$Id: Ascii.Mod,v 1.1 1997/02/07 07:45:32 oberon1 Exp $	*)
(****TLIB keywords*** "%n %v %f" *)
(* "CHANNEL.MOD 1.2 22-Jul-98,04:55:52" *)
MODULE OttAscii;  

(*  Standard short character names for control chars.  
    Copyright (C) 1997, 1998  Michael van Acken
    Copyright (C) 1998  Ian Rae
    This file is part of OTT.

    OTT is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.  

    OTT is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
    or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details. 

    You should have received a copy of the GNU General Public License
    along with OTT. If not, write to the Free Software Foundation, 59
    Temple Place - Suite 330, Boston, MA 02111-1307, USA.

	------------------------------------
	Note. OTT is a modified and extended version of OOC.  The original OOC
	 copyright for this file is:
	    Copyright (C) 1997, 1998  Michael van Acken
    	This file is part of OOC.
	------------------------------------
*)

CONST
  nul* = 00X;   soh* = 01X;   stx* = 02X;   etx* = 03X;
  eot* = 04X;   enq* = 05X;   ack* = 06X;   bel* = 07X;
  bs * = 08X;   ht * = 09X;   lf * = 0AX;   vt * = 0BX;
  ff * = 0CX;   cr * = 0DX;   so * = 0EX;   si * = 0FX;
  dle* = 01X;   dc1* = 11X;   dc2* = 12X;   dc3* = 13X;
  dc4* = 14X;   nak* = 15X;   syn* = 16X;   etb* = 17X;
  can* = 18X;   em * = 19X;   sub* = 1AX;   esc* = 1BX;
  fs * = 1CX;   gs * = 1DX;   rs * = 1EX;   us * = 1FX;
  del* = 7FX;

CONST  (* often used synonyms *)
  sp *  = " ";
  xon*  = dc1;
  xoff* = dc3;
  
END OttAscii.
