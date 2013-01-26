(***********************************************************************)
(* poly.ml - Simple polynomial implementation                          *)
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
module Unix = UnixLabels
open Printf
open Scanf
open ZZp.Infix
module Map = PMap.Map

let rec rfind ~f low high =
  if low >= high then raise Not_found
  else if f(low) then low
  else rfind ~f (low + 1) high

type t = { a : ZZp.zz array;
           (** coefficients, listed from lowest to highest degree *)
           degree : int; (** degree of polynomial *)
         }

let compute_degree a =
  let rec loop a i =
    if i <= 0 then 0
    else (
      if a.(i) =: ZZp.zero
      then loop a (i - 1)
      else i
    )
  in
  loop a (Array.length a - 1)

let init degree ~f =
  let a = Array.init (degree + 1) ~f:(fun i -> f i) in
  let degree = compute_degree a in
  { a = (if degree + 1 < Array.length a
         then Array.sub a ~pos:0 ~len:(degree + 1)
         else a);
    degree = degree;
  }

let make degree x =
  if x =: ZZp.zero then { a = [| ZZp.zero |]; degree = 0; }
  else
    { a = Array.init (degree + 1) ~f:(fun i -> x);
      degree = degree;
    }

let zero = make 0 ZZp.zero
let one = make 0 ZZp.one

(* Get and set coeffs *)
(*let getc x i =  x.a.(i)
  let setc x i v = x.a.(i) <- v
  let lgetc x i = x.a.(i)
  let rgetc x i = x.a.(i) *)
let degree x = x.degree
let length x = Array.length x.a

let copy x = { x with a = Array.copy x.a }

let to_string x =
  let buf = Buffer.create 0 in
  for i = degree x downto 1 do
    bprintf buf "%s z^%d + " (ZZp.to_string x.a.(i)) i;
  done;
  if degree x >= 0
  then bprintf buf "%s" (ZZp.to_string x.a.(0))
  else bprintf buf "0";
  Buffer.contents buf

let splitter = Str.regexp "[ \t]+\\+[ \t]+"

let parse_digit s =
  try sscanf s "%s z^%d" (fun digit degree -> (degree,ZZp.of_string digit))
  with End_of_file -> (0,ZZp.of_string s)

let map_keys map =
  Map.fold ~init:[] ~f:(fun ~key ~data keylist -> key::keylist) map


let of_string s =
  let digits = List.map ~f:parse_digit (Str.split splitter s) in
  let digitmap = Map.of_alist digits in
  let degree = MList.reduce ~f:max (map_keys digitmap) in
  init degree ~f:(fun deg ->
                    try Map.find deg digitmap
                    with Not_found -> ZZp.zero)



let print x =
  for i = degree x downto 1 do
    ZZp.print x.a.(i);
    printf " z^%d + " i;
  done;
  if degree x >= 0 then
    ZZp.print x.a.(0)
  else
    print_string "0"

exception NotEqual

let eq x y =
  try
    if x.degree <> y.degree then raise NotEqual;
    for i = 0 to x.degree do
      if x.a.(i) <>: y.a.(i)
      then raise NotEqual
    done;
    true
  with
      NotEqual -> false


let of_array array =
  if Array.length array = 0 then zero
  else
    let deg = compute_degree array in
    { a = Array.init (deg + 1) ~f:(fun i -> array.(i));
      degree = deg;
    }

let term deg c =
  init ~f:(fun i -> if i = deg then c else ZZp.zero) deg

let set_length length x =
  assert (length + 1 > degree x);
  { a = Array.init (length + 1)
            ~f:(fun i ->
                  if i <= x.degree
                  then x.a.(i)
                  else ZZp.zero);
    degree = x.degree
  }

let to_array x = Array.copy x.a
let is_monic x = x.a.(degree x) =: ZZp.one

let eval poly z =
  let zd = ref ZZp.one
  and sum = ref ZZp.zero in
  for deg = 0 to degree poly do
    sum := !sum +: poly.a.(deg) *: !zd;
    zd := !zd *: z
  done;
  !sum

let mult x y =
  let mdegree = degree x + degree y in
  let prod = { a = Array.make ( mdegree + 1 ) ZZp.zero;
               degree = mdegree ;
             }
  in
  for i = 0 to degree x  do
    for j = 0 to degree y do
      prod.a.(i + j) <- prod.a.(i + j) +: x.a.(i) *: y.a.(j)
    done
  done;
  prod

(** scalar multiplication *)
let scmult x c =
  { x with a = Array.map ~f:(fun z -> z *: c) x.a; }

let add x y =
  let deg = max x.degree y.degree in
  init deg
    ~f:(fun i ->
          (if i <= x.degree then x.a.(i) else ZZp.zero) +:
          (if i <= y.degree then y.a.(i) else ZZp.zero))

let neg x = { x with a = Array.map ~f:(fun c -> ZZp.neg c) x.a }

let sub x y = add x (neg y)

let rec divmod x y =
  if eq x zero then (zero,zero)
  else if degree y > degree x then (zero,x)
  else
    let degdiff = degree x - degree y in
    assert (degdiff >= 0);
    let c = x.a.(degree x) /: y.a.(degree y) in
    let m = term degdiff c in
    let new_x = sub x (mult m y) in
    assert (degree new_x < degree x || degree x = 0);
    let (q,r) = divmod new_x y in
    (add q m,r)

let modulo x y = let (q,r) = divmod x y in r
let div x y = let (q,r) = divmod x y in q

let const_coeff x = x.a.(0)
let nth_coeff x n = x.a.(n)
let const c = make 0 c


let rec gcd_rec x y =
  if eq y zero then x
  else
    let (q,r) = divmod x y in
    gcd_rec y r

let gcd x y =
  let result = gcd_rec x y in
  (* force the GCD to be monic *)
  mult result (const (ZZp.inv result.a.(degree result)))


