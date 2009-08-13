(************************************************************************)
(* This file is part of SKS.  SKS is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA *)
(***********************************************************************)

(** Basic operations and definitions for multi-precistion integers. *)

open Big_int

let two = big_int_of_int 2
let one = unit_big_int
let zero = zero_big_int
let neg_one = big_int_of_int (-1)

let ( *! ) = mult_big_int
let ( +! ) = add_big_int
let ( -! ) = sub_big_int
let ( %! ) = quomod_big_int
let ( /! ) = div_big_int
let ( **! ) = power_big_int_positive_big_int
let ( <>! ) x y = not (eq_big_int x y)
let ( =! ) = eq_big_int
let ( <! ) = lt_big_int
let ( >! ) = gt_big_int
let ( <=! ) = le_big_int
let ( >=! ) = ge_big_int


let width = 8
let width_pow = power_int_positive_int 2 width

let revstring s = 
  let len = String.length s in
  let copy = String.create len in
  for i = 0 to len - 1 do 
    copy.[i] <- s.[len - 1 - i]
  done;
  copy

let revstring_inplace s = 
  let len = String.length s in
  for i = 0 to (len - 2)/2 do
    let j = len - 1 - i in
    let tmp = s.[i] in
    s.[i] <- s.[j];
    s.[j] <- tmp
  done

let bigint_to_bytes ~nbytes n = 
  if sign_big_int n = -1 
  then raise (Invalid_argument "N.to_bytes: negative argument");
  let string = String.create nbytes in
  let rec loop n i = 
    if i < 0 then string
    else  
      let (a,b) = n %! width_pow in
      string.[i] <- char_of_int (int_of_big_int b);
      loop a (i - 1)
  in
  let str = loop n (nbytes - 1) in
  revstring_inplace str;
  str

let bigint_of_bytes str = 
  let str = revstring str in
  let nbytes = String.length str in
  let rec loop n i = 
    if i >= nbytes then n
    else
      let m = big_int_of_int (int_of_char str.[i]) in
      loop (n *! width_pow +! m) (i+1)
  in
  loop zero 0 

module type ZZpType = 
sig
  type t 
  type tref
  type zzarray 
  val nbits : int
  val nbytes : int
  val of_bytes : string -> t
  val to_bytes : t -> string
  val of_int : int -> t
  val to_N : t -> big_int
  val of_N : big_int -> t

  val one : t
  val zero : t

  val add : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val mult : t -> t -> t
  val inv : t -> t
  val neg : t -> t
  val shl : t -> int -> t

  val imult : t -> int -> t

  val add_fast : t -> t -> t
  val mul_fast : t -> t -> t
  val mult_fast : t -> t -> t
  val square : t -> t
  val square_fast : t -> t
  val canonicalize : t -> t

  val sub : t -> t -> t
  val print : t -> unit

  val imul : t -> int -> t

  val lt : t -> t -> bool
  val gt : t -> t -> bool
  val eq : t -> t -> bool
  val neq : t -> t -> bool

  val look : tref -> t
  val mult_in : tref -> t -> t -> unit
  val mult_fast_in : tref -> t -> t -> unit
  val add_in : tref -> t -> t -> unit
  val add_fast_in : tref -> t -> t -> unit
  val sub_in : tref -> t -> t -> unit
  val sub_fast_in : tref -> t -> t -> unit
  val copy_in : tref -> t -> unit
  val copy_out : tref -> t
  val make_ref : t -> tref
  val canonicalize_in : tref -> unit

  val points : int -> t array
  val svalues : int -> zzarray
  val to_string : t -> string

  val add_el_array : points: t array -> t -> t array
  val del_el_array : points: t array -> t -> t array
  val mult_array : svalues: zzarray -> t array -> unit


  val add_el : svalues:zzarray -> points:t array -> 
    t -> unit (* modifies svalues *)
  val del_el : svalues:zzarray -> points:t array -> 
    t -> unit (* modifies svalues *)

  val length : zzarray -> int
  val zzarray_to_array : zzarray -> t array
  val zzarray_of_array : t array -> zzarray 
  val zzarray_div : zzarray -> zzarray -> zzarray
  val zzarray_copy : zzarray -> zzarray

  val cmp : t -> t -> int

  val order : big_int
end

