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

(** Simple polynomial implementation *)
open StdLabels
open MoreLabels
module Unix=UnixLabels

(*module ZZp = RMisc.ZZp*)
module Nx = Number.Nx
open Printf

let rec rfind ~f low high = 
  if low >= high then raise Not_found
  else if f(low) then low
  else rfind ~f (low + 1) high

type t = { a : ZZp.tref array;
	   mutable degree : int;
	 } 

let init ~degree ~f = 
  { a = Array.init (degree + 1) ~f:(fun i -> ZZp.make_ref (f i));
    degree = degree;
  }

let make ~degree x = 
  { a = Array.init (degree + 1) ~f:(fun i -> ZZp.make_ref x);
    degree = degree;
  }

(* Get and set coeffs *)
let getc x i = ZZp.copy_out x.a.(i)
let setc x i v = ZZp.copy_in x.a.(i) v
let lgetc x i = ZZp.look x.a.(i)
let rgetc x i = x.a.(i)
let degree x = x.degree
let length x = Array.length x.a

let zero = make ~degree:0 ZZp.zero
let one = make ~degree:0 ZZp.one
let copy x = { x with a = Array.init (x.degree + 1)
			    ~f:(fun i -> ZZp.make_ref (ZZp.look x.a.(i))); }

let print x = 
  for i = degree x downto 1 do
    ZZp.print (getc x i);
    printf " z^%d + " i;
  done;
  if degree x >= 0 then
    ZZp.print (getc x 0)
  else
    print_string "0"


let copy_in r x = 
  if ( degree x >= Array.length r.a ||
       degree x >= Array.length x.a )
  then failwith "Poly.copy_in: Assertion failure";
  r.degree <- x.degree;
  for i = 0 to x.degree do
    setc r i (lgetc x i)
  done

(*Array.map ~f:(fun v -> ZZp.make_ref (ZZp.look v)) x.a } *)



exception NotEqual 

let eq x y = 
  try
    if x.degree <> y.degree then raise NotEqual;
    for i = 0 to x.degree do
      if lgetc x i <> lgetc y i
      then raise NotEqual
    done;
    true
  with
      NotEqual -> false


let canonicalize x = 
  for i = 0 to degree x do
    let v = rgetc x i in 
    ZZp.canonicalize_in v
  done

let rec compute_degree_rec a i = 
  if i <= 0 then 0
  else (
    if ZZp.look a.(i) <> ZZp.zero 
    then i
    else compute_degree_rec a (i - 1)
  )

let compute_degree x = 
  compute_degree_rec x.a x.degree

let rec shrink x = 
  { x with a = Array.init (x.degree + 1) ~f:(fun i -> x.a.(i)) }

let degshrink x = 
  let x = { x with degree = compute_degree x } in
  shrink x

let reset_degree x = 
  x.degree <- compute_degree x

let of_array array = 
  let a = Array.init (Array.length array) 
	    ~f:(fun i -> ZZp.make_ref array.(i)) in
  let x = { a = a;
	    degree = Array.length a - 1; 
	  } 
  in
  x.degree <- compute_degree x;
  x

let set_length length x = 
  assert (length + 1 > degree x);
  { a = Array.init (length + 1)
	    ~f:(fun i -> 
		  if i <= x.degree 
		  then ZZp.make_ref (lgetc x i)
		  else ZZp.make_ref ZZp.zero);
    degree = x.degree
  }

let to_array x = Array.init ~f:(fun i -> ZZp.copy_out x.a.(i)) (degree x)
let is_monic x = lgetc x (degree x) = ZZp.one

let eval poly z = 
  let zd = ZZp.make_ref ZZp.one
  and sum = ZZp.make_ref ZZp.zero in
  for deg = 0 to degree poly do 
    ZZp.add_in sum (ZZp.look sum) 
      (ZZp.mult_fast (ZZp.look poly.a.(deg)) (ZZp.look zd));
    ZZp.mult_in zd (ZZp.look zd) z
  done;
  ZZp.copy_out sum

let mult x y = 
  let mdegree = degree x + degree y in
  let prod = { a = Array.init ( mdegree + 1 ) 
		     ~f:(fun i -> ZZp.make_ref ZZp.zero);
	       degree = mdegree ;
	     }
  in
  for i = 0 to degree x  do
    for j = 0 to degree y do
      let v = rgetc prod (i + j) in
      ZZp.add_fast_in v (ZZp.look v) (ZZp.mult_fast (lgetc x i) (lgetc y j))
    done
  done;
  canonicalize prod;
  prod


let mult_in result x y = 
  let mdegree = degree x + degree y in
  result.degree <- mdegree;
  for i = 0 to mdegree do 
    ZZp.copy_in (rgetc result i) ZZp.zero
  done;
  for i = 0 to degree x  do
    for j = 0 to degree y do
      let v = rgetc result (i + j) in
      ZZp.add_fast_in v (ZZp.look v) (ZZp.mult_fast (lgetc x i) (lgetc y j))
    done
  done;
  canonicalize result

(* in-place multiply of term of the form (Z + a) *)
let lmult_in result x a = 
  let mdegree = degree x + 1 in
  result.degree <- mdegree;
  (* first, copy in shifted-up version of x *)
  for i = 0 to degree x do 
    ZZp.copy_in (rgetc result (i+1)) (lgetc x i)
  done;
  ZZp.copy_in (rgetc result 0) ZZp.zero;

  for i = 0 to degree x  do
    let v = rgetc result i in
    ZZp.add_in v (ZZp.look v) (ZZp.mult_fast (lgetc x i) a)
  done


let square_in result x = 
  let mdegree = (degree x) * 2 in
  result.degree <- mdegree;
  for i = 0 to mdegree do 
    ZZp.copy_in (rgetc result i) ZZp.zero
  done;
  for i = 0 to degree x  do
    let v = rgetc result (2 * i) in
    ZZp.add_fast_in v (ZZp.look v) (ZZp.square_fast (lgetc x i));
    for j = i + 1 to degree x do
      let v = rgetc result (i + j) in
      let m = ZZp.mult_fast (lgetc x i) (lgetc x j) in
      ZZp.add_fast_in v (ZZp.look v) m;
      ZZp.add_fast_in v (ZZp.look v) m
    done
  done;
  canonicalize result


let add x y = 
  let deg_x = degree x
  and deg_y = degree y in
  let sum =  make ~degree:(max deg_x deg_y) ZZp.zero in
  let mindeg = min deg_x deg_y in
  for i = 0 to mindeg do
    let v = rgetc sum i in
    ZZp.add_in v (lgetc x i) (lgetc y i)
  done;
  let larger = if deg_x > deg_y then x else y in
  for i = mindeg + 1 to max deg_x deg_y do
    ZZp.copy_in (rgetc sum i) (lgetc larger i)
  done;
  reset_degree sum;
  sum


let scmult_in x c = 
  for i = 0 to degree x do
    let v = rgetc x i in ZZp.mult_in v (ZZp.look v) c
  done

let scmult x c = 
  let y = copy x in
  scmult_in y c;
  y


let sub x y = 
  let deg_x = degree x
  and deg_y = degree y in
  let sum =  make ~degree:(max deg_x deg_y) ZZp.zero in
  let mindeg = min deg_x deg_y in
  for i = 0 to mindeg do
    let v = rgetc sum i in
    ZZp.sub_in v (lgetc x i) (lgetc y i)
  done;
  if deg_x > deg_y
  then
    for i = mindeg + 1 to deg_x do
      ZZp.copy_in (rgetc sum i) (lgetc x i)
    done
  else
    for i = mindeg + 1 to deg_y do
      ZZp.copy_in (rgetc sum i) (ZZp.neg (lgetc x i))
    done;
  reset_degree sum;
  sum

let divmod x y = (* computes (q,r) s.t. y * q + r = x *)
  if degree y > degree x 
  then 
    let (q,r) = (zero,copy x) in (q,r)
  else
    let r = copy x in
    let degdiff = degree r - degree y in
    let q = make ~degree:degdiff ZZp.zero in
    for i = degree r downto degree r - degdiff do 
      let c = ZZp.div (lgetc r i) (lgetc y (degree y)) in
      setc q (degdiff - (degree r - i)) c;
      if c = ZZp.zero then ()
      else
	for j = degree y downto 0 do
	  let dest = i - (degree y - j) in
	  let v = rgetc r dest in
	  ZZp.sub_in v (ZZp.look v) (ZZp.mult_fast c (lgetc y j))
	done
    done;
    reset_degree q;
    reset_degree r;
    (q,r) 
    (* (degshrink q, degshrink r) *)


let divmod_in ~q ~r x y = (* computes (q,r) s.t. y * q + r = x *)
  if degree y > degree x 
  then ( 
    copy_in r x;
    copy_in q zero; 
  )
  else ( 
    copy_in r x;
    let degdiff = degree r - degree y in
    q.degree <- degdiff;
    for i = degree r downto degree r - degdiff do 
      let c = ZZp.div (lgetc r i) (lgetc y (degree y)) in
      setc q (degdiff - (degree r - i)) c;
      if c = ZZp.zero then ()
      else
	for j = degree y downto 0 do
	  let dest = i - (degree y - j) in
	  let v = rgetc r dest in
	  ZZp.sub_in v (ZZp.look v) (ZZp.mult_fast c (lgetc y j));
	done
    done;
    reset_degree q; 
    reset_degree r;
  )


let modulo x y = let (q,r) = divmod x y in r
let div x y = let (q,r) = divmod x y in q

let deriv x = 
  Array.init (degree x) ~f:(fun i -> ZZp.imult (getc x (i+1)) (i+1)) 

let const_coeff x = (getc x 0)
let nth_coeff x n = (getc x n)



(*
let rec gcd_rec ~q ~r x y = 
  if eq y zero then x
  else (
    divmod_in ~q ~r x y;
    gcd_rec ~q ~r:x y r
  )

let gcd x y = 
  let mdegree = max (degree x) (degree y) in
  let q = set_length mdegree zero in
  let r = set_length mdegree zero in
  let result = gcd_rec ~q ~r (set_length mdegree x) (set_length mdegree y) in
  scmult_in result (ZZp.inv (lgetc result (degree result)));
  result
*)

(***************************************)

let rec gcd_rec x y = 
  if eq y zero then x
  else
    let (q,r) = divmod x y in
    gcd_rec y r
      
let gcd x y = 
  let result = gcd_rec x y in
  scmult_in result (ZZp.inv (lgetc result (degree result)));
  result
