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

open Int_comparators

(** Field of integers mod p (for a settable prime p) *)
open StdLabels
open MoreLabels
module Unix = UnixLabels
open Printf

module Nx = Number.Nx

type t = Nx.t
type tref = Nx.tref
type zzarray = tref array

let one = Number.one
let zero = Number.zero

let order = ref Number.two
let nbits = ref 0
let nbytes = ref 0

let set_order value = 
  order := value;
  nbits := Nx.nbits !order;
  nbytes := !nbits / 8 + (if !nbits mod 8 = 0 then 0 else 1)


open Number.Infix

let modulo = Nx.modulo

let num_bytes () = !nbytes
let of_bytes bytes = Number.of_bytes bytes
let to_bytes n = Number.to_bytes ~nbytes:!nbytes (modulo n !order)
let of_int i  = modulo (Nx.of_int i) !order
let to_N x = x
let of_N x = modulo x !order

let add x y = modulo (x ++ y) !order
let mul x y = modulo (x ** y) !order
let mult x y = modulo (x ** y) !order
let imult x y = modulo (Nx.mul_1 x y) !order

let add_fast x y = (x ++ y) 
let mul_fast x y = (x ** y) 
let mult_fast x y = (x ** y) 
let canonicalize x = modulo x !order

let shl x i = Nx.shl x i

let square x = Nx.modulo (Nx.sqr x) !order
let square_fast x = Nx.sqr x

let lt x y = Nx.inf x y
let gt x y = Nx.sup x y
let eq x y = Nx.eq x y
let neq x y = Nx.neq x y

let imul x y = modulo (Nx.mul_1 x y) !order
let neg x = Nx.sub !order x
let inv x = 
  if eq x zero then raise (Invalid_argument "ZZp.inv: Attempt to invert 0");
  let (u,_,_) = Nx.gcd_ex x !order in modulo u !order 

let div x y = modulo (x ** (inv y)) !order
let sub x y = modulo (x -- y) !order


let to_string x = Nx.string_of x
let of_string s = Nx.of_string s
let print x = print_string (to_string x)

let points n = 
  Array.init n ~f:(fun i -> 
    let ival = ((i + 1) / 2) * (if i mod 2 = 0 then 1 else (-1))
    in Nx.of_int ival)

let svalues n = 
  Array.init n ~f:(fun i -> Nx.make_ref one)


(* In-place operations *)
let tmp = Nx.make_ref zero

let look v = Nx.look v
	       
let mult_in v x y = 
  Nx.mul_in v x y;
  Nx.quomod_in tmp v (Nx.look v) !order

let mult_fast_in v x y = Nx.mul_in v x y

let add_in v x y = 
  Nx.add_in v x y;
  Nx.quomod_in tmp v (Nx.look v) !order

let add_fast_in v x y = 
  Nx.add_in v x y

let sub_in v x y = 
  Nx.sub_in v x y;
  Nx.quomod_in tmp v (Nx.look v) !order

let sub_fast_in v x y = Nx.sub_in v x y

let div_in x y = mult_in x (inv y)

let copy_in v x = Nx.copy_in v x
let copy_out v = Nx.copy_out v
let make_ref x = Nx.make_ref x

let canonicalize_in v = Nx.quomod_in tmp v (Nx.look v) !order


(* Array-wise functions for adding elements to svalues *)
			  
let add_el_array ~points el = 
  Array.init (Array.length points) 
    ~f:( fun i -> 
	   let rval = modulo (points.(i) -- el) !order in
	   if eq rval zero 
	   then failwith "Sample point added to set"
	   else rval )

let del_el_array ~points el = 
  Array.init (Array.length points) 
    ~f:( fun i -> 
	   let rval = inv (points.(i) -- el) in
	   if eq rval zero 
	   then failwith "Sample point added to set"
	   else rval)

let mult_array ~svalues array =
  if Array.length svalues <> Array.length array 
  then raise (Invalid_argument "ZZp.add_el: array lengths don't match");
  for i = 0 to Array.length array - 1 do 
    let v = svalues.(i) in
    mult_in v ~~v array.(i)
  done
  

(* Element-based functions for adding elements to svalues *)

let add_el ~svalues ~points el = 
  if Array.length svalues <> Array.length points 
  then raise (Invalid_argument "ZZp.add_el: array lengths don't match");
  for i = 0 to Array.length points - 1 do 
    let v = svalues.(i) in
    mult_in v ~~v (points.(i) -- el)
  done

(* needs checking *)
let del_el ~svalues ~points el = 
  if Array.length svalues <> Array.length points 
  then raise (Invalid_argument "ZZp.del_el: array lengths don't match");
  for i = 0 to Array.length points - 1 do 
    let v = svalues.(i) in
    div_in v ~~v (points.(i) -- el)
  done

let array_mult x y = 
  let len = Array.length x in
  Array.init len ~f:(fun i -> mult x.(i) y.(i))

let zzarray_div x y = 
  let len = Array.length x in
  Array.init len ~f:(fun i -> Nx.make_ref (div ~~(x.(i)) ~~(y.(i))))

let zzarray_copy ar = 
  Array.map ~f:(fun zz -> Nx.make_ref (Nx.look zz)) ar

let cmp x y = Nx.cmp x y

let length array = Array.length array

let zzarray_to_array array = Array.map ~f:(fun x -> copy_out x) array
let zzarray_of_array array = Array.map ~f:(fun x -> make_ref x) array

let to_string_array x = 
  Array.init 1 ~f:(fun i -> to_bytes x)
