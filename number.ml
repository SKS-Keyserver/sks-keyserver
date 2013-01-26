(***********************************************************************)
(* number.ml - Basic operations and definitions for multi-precision    *)
(*             integers                                                *)
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

open Big_int
open StdLabels
open MoreLabels
open Printf
open Common

type z = Big_int.big_int

module Infix =
struct
  let two = big_int_of_int 2
  let one = unit_big_int
  let zero = zero_big_int
  let neg_one = big_int_of_int (-1)

  let ( *! ) = mult_big_int
  let ( +! ) = add_big_int
  let ( -! ) = sub_big_int
  let ( %! ) = mod_big_int
  let ( /! ) = div_big_int
  let ( **! ) = power_big_int_positive_int
  let ( <>! ) x y = not (eq_big_int x y)
  let ( =! ) = eq_big_int
  let ( <! ) = lt_big_int
  let ( >! ) = gt_big_int
  let ( <=! ) = le_big_int
  let ( >=! ) = ge_big_int
end

open Infix

let int_mult = mult_int_big_int
let int_posint_power = power_int_positive_int

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

let to_bytes ~nbytes n =
  if sign_big_int n = -1
  then raise (Invalid_argument "N.to_bytes: negative argument");
  let string = String.create nbytes in
  let rec loop n i =
    if i < 0 then string
    else
      let (a,b) = quomod_big_int n width_pow in
      string.[i] <- char_of_int (int_of_big_int b);
      loop a (i - 1)
  in
  let str = loop n (nbytes - 1) in
  revstring_inplace str;
  str

let of_bytes str =
  let str = revstring str in
  let nbytes = String.length str in
  let rec loop n i =
    if i >= nbytes then n
    else
      let m = big_int_of_int (int_of_char str.[i]) in
      loop (n *! width_pow +! m) (i+1)
  in
  loop zero 0



open Big_int
open Nat

let nbits_slow x =
  let rec loop i two_to_i =
    if two_to_i >! x then i
    else loop (succ i) (two *! two_to_i)
  in
  if x =! zero then 1 else loop 1 two

let nbits_less_slow x =
  let nwords = num_digits_big_int x in
  let wsize = Sys.word_size in
  let lowbits = (nwords - 1) * wsize in
  let lastword = x /! two **! lowbits in
  nbits_slow lastword + (nwords - 1) * wsize

(** returns the number of bits required to represent the number, i.e.,
  the index (starting from 1) of the most significant non-zero bit *)
let nbits x =
 let nat = nat_of_big_int (abs_big_int x) in
 let nwords = num_digits_nat nat 0 (length_nat nat) in
 Sys.word_size * nwords - num_leading_zero_bits_in_digit nat (nwords - 1)

let nth_bit x n =
  one =! ( x /! (two **! n)) %! two

let print_bits x =
  for i = nbits x - 1 downto 0 do
    if nth_bit x i then print_string "1" else print_string "0"
  done

let squaremod x m =
  (x *! x) %! m

let rec powmod x y m =
  if y =! zero then one
  else
    let base = squaremod (powmod x ( y /! two) m) m in
    if y %! two =! zero then base
    else (base *! x) %! m

let dumb_powmod x y m =
  (x **! int_of_big_int y) %! m

let rec gcd_ex' a b =
  if b =! zero then (one,zero,a)
  else
    let (q,r) = quomod_big_int a b in
    let (u',v',gcd) = gcd_ex' b r in
    (v',u' -! v' *! q, gcd)

let gcd_ex a b =
  if b <=! a then gcd_ex' a b
  else
    let (u,v,gcd) = gcd_ex' b a in
    (v,u,gcd)

let gcd_ex_test a b =
     let (a,b) = (big_int_of_int a,big_int_of_int b) in
     let (u,v,gcd) = gcd_ex a b in
     if (u *! a +! v *! b <>! gcd)
     then failwith (sprintf "gcd_ex failed on %s and %s"
                      (string_of_big_int a) (string_of_big_int b))


(** conversion functions *)

let of_int = big_int_of_int
let to_int = int_of_big_int
let to_string = string_of_big_int
let of_string = big_int_of_string
let compare = compare_big_int


