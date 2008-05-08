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

(** Field of integers mod p (for a settable prime p) *)
open StdLabels
open MoreLabels
module Unix=UnixLabels
open Printf

open Number2
open Big_int

type t = big_int
type tref = big_int ref
type zzarray = big_int array

let order = ref two
let nbits = ref 0
let nbytes = ref 0

let rec num_bits x = 
  if x =! zero then 0 
  else 1 + num_bits (x /! two)

let set_order value = 
  order := value;
  nbits := num_bits !order;
  nbytes := !nbits / 8 + (if !nbits mod 8 = 0 then 0 else 1)



let modulo = mod_big_int

let num_bytes () = !nbytes
let of_bytes bytes = bigint_of_bytes bytes
let to_bytes n = bigint_to_bytes ~nbytes:!nbytes (modulo n !order)
let of_int i  = modulo (big_int_of_int i) !order
let to_N x = x
let of_N x = modulo x !order

let add x y = modulo (x +! y) !order
let mul x y = modulo (x *! y) !order
let mult x y = modulo (x *! y) !order
let imult x y = modulo (mult_int_big_int x y) !order

let add_fast x y = (x +! y) 
let mul_fast x y = (x *! y) 
let mult_fast x y = (x *! y) 
let canonicalize x = modulo x !order

let shl x i = 
  x *! power_int_positive_int 2 i

let square x = modulo (x *! x) !order
let square_fast x = x *! x

let imul x y = modulo (mult_big_int y x) !order
let neg x = !order -! x
let inv x = 
  if x = zero then raise (Invalid_argument "ZZp.inv: Attempt to invert 0");
  let u = gcd_big_int x !order in 
  modulo u !order 

let div x y = modulo (x *! (inv y)) !order
let sub_fast x y = x -! y
let sub x y = modulo (x -! y) !order

let lt = lt_big_int
let gt = gt_big_int
let eq = eq_big_int
let neq x y = not (eq_big_int x y)

let to_string x = string_of_big_int x
let print x = print_string (to_string x)

let points n = 
  Array.init n 
    ~f:(fun i -> 
	  let ival = ((i + 1) / 2) * (if i mod 2 = 0 then 1 else (-1)) in
	  big_int_of_int ival)

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

let canonicalize_in v = v := modulo !v !order

(* Array-wise functions for adding elements to svalues *)
			  
let add_el_array ~points el = 
  Array.init (Array.length points) 
    ~f:( fun i -> 
	   let rval = modulo (points.(i) -! el) !order in
	   if eq rval zero 
	   then failwith "Sample point added to set"
	   else rval )

let del_el_array ~points el = 
  Array.map ~f:inv (add_el_array ~points el)

let mult_array ~svalues array =
  if Array.length svalues <> Array.length array 
  then raise (Invalid_argument "ZZp.add_el: array lengths don't match");
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

let zzarray_div x y = 
  Array.init (Array.length x) ~f:(fun i -> x.(i) /! y.(i))

let zzarray_copy ar = Array.copy ar

let cmp = compare_big_int

let length array = Array.length array

let zzarray_to_array array = Array.copy array
let zzarray_of_array array = Array.copy array

let to_string_array x = 
  Array.init 1 ~f:(fun i -> to_bytes x)
