(***********************************************************************)
(* decode.ml - Handles decoding aspect of set-reconciliation           *)
(*             algorithm.                                              *)
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

open ZZp.Infix
open StdLabels
open MoreLabels
module Unix=UnixLabels
open Printf

module ZSet = ZZp.Set
open LinearAlg
open ZZp.Infix

exception Low_mbar
exception Interpolation_failure


(** takes [values], an array of evaluations of an unknown rational function,
  evaluated at [points], and [d], the degree difference between the numerator
  and the denominator.  Returns the numerator,denominator pair describing the
  reduced rational function, if such exists.
*)
let interpolate ~values ~points ~d =
  if (abs d) > Array.length values
  then raise Interpolation_failure;
  let mbar = Array.length values in
  let mbar =
    if (mbar + d) mod 2 <> 0
    then mbar - 1 else mbar
  in
  let ma = (mbar + d) / 2 and mb = (mbar - d) / 2 in
  let matrix = Matrix.make ~rows:mbar ~columns:(mbar + 1)
                 ZZp.zero in
  for j = 0 to mbar - 1 do
    let accum = ref ZZp.one in
    let kj = points.(j) in
    let fj = values.(j) in

    for i = 0 to ma - 1 do
      Matrix.set matrix i j !accum;
      accum := ZZp.mul kj !accum
    done;
    let kjma = !accum in

    accum := ZZp.neg fj;
    for i = ma  to mbar - 1 do
      Matrix.set matrix i j !accum;
      accum := ZZp.mul kj !accum
    done;
    let fjkjmb = ZZp.neg !accum in

    Matrix.set matrix mbar j (ZZp.sub fjkjmb kjma)
  done;

  (try reduce matrix
   with Failure s -> raise Interpolation_failure);

  let acoeffs = Array.init (ma + 1)
                  ~f:(fun j -> if j = ma then ZZp.one
                        else Matrix.get matrix mbar j)
  and bcoeffs = Array.init (mb + 1)
                  ~f:(fun j -> if j = mb then ZZp.one
                      else Matrix.get matrix mbar (j + ma)) in
  let apoly = Poly.of_array acoeffs and bpoly = Poly.of_array bcoeffs in
  let g = Poly.gcd apoly bpoly in
  (Poly.div apoly g, Poly.div bpoly g)



(*********************************************************************)
(*********************************************************************)

let mult modulus x y = Poly.modulo (Poly.mult x y) modulus
let square modulus x = Poly.modulo (Poly.mult x x) modulus

let powmod ~modulus x n =
  let nbits = Number.nbits n in
  let rval = ref Poly.one in
  let x2n = ref x in
  for bit = 0 to nbits do
    if Number.nth_bit n bit then
      rval := mult modulus !rval !x2n;
    x2n := square modulus !x2n
  done;
  !rval

(************************************************************)

let rand_ZZp () =
  let primebits = !ZZp.nbits in
  let random = Prime.randbits Random.bits primebits in
  ZZp.of_number random

(** Checks preconditions of factorizability.  In particular, that the
    polynomial is *)
let factor_check x =
  if Poly.degree x = 1 || Poly.degree x = 0 then true
  else
    let z = Poly.of_array [| ZZp.zero; ZZp.one |] in
    let zq = powmod ~modulus:x z !ZZp.order in
    let mz = Poly.scmult z (ZZp.of_int (-1)) in
    let zqmz = Poly.modulo (Poly.add zq mz) x in
    Poly.eq zqmz Poly.zero

let gen_splitter f =
  let q =  ZZp.neg ZZp.one /: ZZp.two in
  let a =  rand_ZZp () in
  let za = Poly.of_array [| a ; ZZp.one |] in
  let zaq = powmod ~modulus:f za (ZZp.to_number q) in
  let zaqo = Poly.sub zaq Poly.one in
  zaqo

let rec rand_split f =
  let splitter = gen_splitter f in
  let first = Poly.gcd splitter f in
  let second = Poly.div f first in
  (first,second)

let rec factor f =
  let degree = Poly.degree f in
  if degree = 1
  then ZSet.add (ZZp.neg (Poly.const_coeff f)) ZSet.empty
  else if degree = 0
  then ZSet.empty
  else
    let (f1,f2) = rand_split f in
    flush stdout;
    ZSet.union (factor f1) (factor f2)

let shorten array =
  Array.init (Array.length array - 1) ~f:(fun i -> array.(i))

let reconcile ~values ~points ~d =
  let len = Array.length points in
  let (num,denom) =
    try interpolate
      ~values:(shorten values)
      ~points:(shorten points) ~d
    with Interpolation_failure -> raise Low_mbar
  in
  let val_from_poly = ZZp.div (Poly.eval num points.(len - 1))
                    (Poly.eval denom points.(len - 1)) in
  if val_from_poly <>: values.(len - 1)  ||
    not (factor_check num) || not (factor_check denom)
  then raise Low_mbar;
  let aset = factor num
  and bset = factor denom
  in (aset,bset)

let array_to_set array =
  Array.fold_left ~f:(fun set el -> ZSet.add el set) ~init:ZSet.empty array

