(***********************************************************************)
(* number.mli - Basic operations and definitions for multi-precision   *)
(*              integers                                               *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012, 2013  Yaron Minsky and Contributors       *)
(*                                                                     *)
(* This file is part of SKS.  SKS is free software; you can            *)
(* redistribute it and/or modify it under the terms of the GNU General *)
(* Public License as published by the Free Software Foundation; either *)
(* version 2 of the License, or (at your option) any later version.    *)
(*                                                                     *)
(* This program is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *)
(* General Public License for more details.                            *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with this program; if not, write to the Free Software         *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA or see <http://www.gnu.org/licenses/>.                          *)
(***********************************************************************)

type z
module Infix :
sig
  val two : z
  val one : z
  val zero : z
  val neg_one : z
  val ( *! ) : z -> z -> z
  val ( +! ) : z -> z -> z
  val ( -! ) : z -> z -> z
  val ( %! ) : z -> z -> z
  val ( /! ) : z -> z -> z
  val ( **! ) : z -> int -> z
  val ( <>! ) : z -> z -> bool
  val ( =! ) : z -> z -> bool
  val ( <! ) : z -> z -> bool
  val ( >! ) : z -> z -> bool
  val ( <=! ) : z -> z -> bool
  val ( >=! ) : z -> z -> bool
end
val width : int
val width_pow : z
val nbits : z -> int
val nth_bit : z -> int -> bool
val print_bits : z -> unit
val squaremod : z -> z -> z
val powmod : z -> z -> z -> z
val dumb_powmod : z -> z -> z -> z
val gcd_ex : z -> z -> z * z * z

val int_mult : int -> z -> z
val int_posint_power : int -> int -> z

(** conversion functions *)

val to_bytes : nbytes:int -> z -> string
val of_bytes : string -> z
val of_int : int -> z
val to_int : z -> int
val to_string : z -> string
val of_string : string -> z
val compare : z -> z -> int
