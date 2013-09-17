(***********************************************************************)
(* zZp.ml - Field of integers mod p (for a settable prime p)           *)
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

open StdLabels
open MoreLabels
module Unix=UnixLabels
open Printf

module N = Number
open Number.Infix
(* open Big_int *)

type zz = Number.z
type zzref = Number.z ref
type mut_array = zz array

let order = ref two
let nbits = ref 0
let nbytes = ref 0

let two = two
let zero = zero
let one = one

let set_order value =
  order := value;
  nbits := N.nbits !order;
  nbytes := !nbits / 8 + (if !nbits mod 8 = 0 then 0 else 1)

let num_bytes () = !nbytes
let of_bytes bytes = N.of_bytes bytes
let to_bytes n = N.to_bytes ~nbytes:!nbytes (n %! !order)
let of_int i  = (Number.of_int i) %! !order
let to_N x = x
let of_N x = x %! !order

let add x y = (x +! y) %! !order
let sub x y = (x -! y) %! !order
let mul x y = (x *! y) %! !order
let mult x y = (x *! y) %! !order
let imult x y = (Number.int_mult y x) %! !order



let add_fast x y = (x +! y)
let mul_fast x y = (x *! y)
let mult_fast x y = (x *! y)
let canonicalize x = x %! !order

let shl x i =
  x *! Number.int_posint_power 2 i

let square x = (x *! x) %! !order
let square_fast x = x *! x

let imul x y = (y *! x) %! !order
let neg x = !order -! x

let inv x =
  if x =! zero then raise (Invalid_argument "ZZp.inv: Attempt to invert 0");
  let (u,_,_) = N.gcd_ex x !order in u %! !order


let div x y = (x *! (inv y)) %! !order
let sub_fast x y = x -! y

let lt = ( <! )
let gt = ( >! )
let eq = ( =! )
let neq x y = not (x =! y)

let to_string = Number.to_string
let of_string = Number.of_string
let print x = print_string (to_string x)

let points n = Array.init n
  ~f:(fun i ->
        let ival = ((i + 1) / 2) * (if i mod 2 = 0 then 1 else (-1)) in
        Number.of_int ival)

let svalues n =
  Array.init n ~f:(fun i -> one)

(* In-place operations.  Since we're using Big_int, there are no in-place operations,
   so we just fake it. *)

let mult_in v x y =
  v := mult x y

let mult_fast_in v x y =
  v := mult_fast x y

let add_in v x y =
  v := add x y

let add_fast_in v x y =
  v := add_fast x y

let sub_in v x y =
  v := sub x y

let sub_fast_in v x y =
  v := x -! y

let copy_in v x = v := x
let copy_out v = !v
let make_ref x = ref x
let look = copy_out

let canonicalize_in v = v := !v %! !order

(* Array-wise functions for adding elements to svalues *)

let add_el_array ~points el =
  Array.init (Array.length points)
    ~f:( fun i ->
           let rval = (points.(i) -! el) %! !order in
           if eq rval zero
           then failwith "Sample point added to set"
           else rval )

let del_el_array ~points el =
  Array.map ~f:inv (add_el_array ~points el)

let mult_array ~svalues array =
  if Array.length svalues <> Array.length array
  then raise (Invalid_argument "ZZp.mult_array: array lengths don't match");
  for i = 0 to Array.length array - 1 do
    svalues.(i) <- mult svalues.(i) array.(i)
  done

(** Element-based functions for adding elements to svalues *)

let add_el ~svalues ~points el =
  if Array.length svalues <> Array.length points
  then raise (Invalid_argument "ZZp.add_el: array lengths don't match");
  for i = 0 to Array.length points - 1 do
    svalues.(i) <- mult svalues.(i) (points.(i) -! el)
  done

(* needs checking *)
let del_el ~svalues ~points el =
  if Array.length svalues <> Array.length points
  then raise (Invalid_argument "ZZp.del_el: array lengths don't match");
  for i = 0 to Array.length points - 1 do
    svalues.(i) <- div svalues.(i) (points.(i) -! el)
  done

let array_mult x y =
  let len = Array.length x in
  Array.init len ~f:(fun i -> mult x.(i) y.(i))

let mut_array_div x y =
  Array.init (Array.length x) ~f:(fun i -> div x.(i) y.(i))

let mut_array_copy ar = Array.copy ar

let cmp = Number.compare

let length array = Array.length array

let mut_array_to_array array = Array.copy array
let mut_array_of_array array = Array.copy array

let to_string_array x =
  Array.init 1 ~f:(fun i -> to_bytes x)

module Set = Set.Make(struct
                        type t = zz
                        let compare = Number.compare
                      end)

let zset_of_list list =
  List.fold_left ~init:Set.empty
    ~f:(fun x y -> Set.add y x) list


let of_number x = x
let canonical_of_number x =
  x %! !order

let to_number x = x

let rand bits =
  let n = Prime.randint bits !order in
  n %! !order

module Infix =
struct
  let ( +: ) = add
  let ( -: ) = sub
  let ( *: ) = mul
  let ( /: ) = div
  let ( =: ) = ( =! )
  let ( <>: ) = ( <>! )
end
