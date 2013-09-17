(***********************************************************************)
(* zZp.mli - Field of integers mod p (for a settable prime p)          *)
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

type zz
type zzref
type mut_array
val order : Number.z ref
val nbits : int ref
val nbytes : int ref
val two : zz
val zero : zz
val one : zz
val set_order : zz -> unit
val num_bytes : unit -> int
val of_bytes : string -> zz
val to_bytes : zz -> string
val of_int : int -> zz
val to_N : 'a -> 'a
val of_N : zz -> zz
val add : zz -> zz -> zz
val sub : zz -> zz -> zz
val mul : zz -> zz -> zz
val mult : zz -> zz -> zz
val imult : zz -> int -> zz
val add_fast : zz -> zz -> zz
val mul_fast : zz -> zz -> zz
val mult_fast : zz -> zz -> zz
val canonicalize : zz -> zz
val square : zz -> zz
val square_fast : zz -> zz
val imul : zz -> zz -> zz
val neg : zz -> zz
val inv : zz -> zz
val div : zz -> zz -> zz
(* val sub_fast : zz -> zz -> zz *)
val lt : zz -> zz -> bool
val gt : zz -> zz -> bool
val eq : zz -> zz -> bool
val neq : zz -> zz -> bool
val to_string : zz -> string
val of_string : string -> zz
val print : zz -> unit
val points : int -> zz array
val svalues : int -> mut_array
val mult_in : zzref -> zz -> zz -> unit
(* val mult_fast_in : zzref -> zz -> zz -> unit *)
val add_in : zzref -> zz -> zz -> unit
(* val add_fast_in : zzref -> zz -> zz -> unit *)
val sub_in : zzref -> zz -> zz -> unit
(* val sub_fast_in : zzref -> zz -> zz -> unit *)
val copy_in : zzref -> zz -> unit
val copy_out : zzref -> zz
val make_ref : zz -> zzref
val look : zzref -> zz
val canonicalize_in : zzref -> unit
val add_el_array : points: zz array -> zz -> zz array
val del_el_array : points: zz array -> zz -> zz array
val mult_array : svalues:mut_array -> zz array -> unit
val add_el : svalues:mut_array -> points:zz array -> zz -> unit
val del_el : svalues:mut_array -> points:zz array -> zz -> unit
val array_mult : zz array -> zz array -> zz array
val mut_array_div : mut_array -> mut_array -> zz array
val mut_array_copy : mut_array -> mut_array
val cmp : zz -> zz -> int
val length : mut_array -> int
val mut_array_to_array : mut_array -> zz array
val mut_array_of_array : zz array -> mut_array
val to_string_array : zz -> string array
val rand : (unit -> int) -> zz

(** Set specialized to ZZp.zz *)
module Set :
  sig
    type elt = zz
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : f:(elt -> unit) -> t -> unit
    val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
    val for_all : f:(elt -> bool) -> t -> bool
    val exists : f:(elt -> bool) -> t -> bool
    val filter : f:(elt -> bool) -> t -> t
    val partition : f:(elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
val zset_of_list : zz list -> Set.t
val canonical_of_number : Number.z -> zz
val of_number : Number.z -> zz
val to_number : zz -> Number.z
module Infix :
  sig
    val ( +: ) : zz -> zz -> zz
    val ( -: ) : zz -> zz -> zz
    val ( *: ) : zz -> zz -> zz
    val ( /: ) : zz -> zz -> zz
    val ( =: ) : zz -> zz -> bool
    val ( <>: ) : zz -> zz -> bool
  end
