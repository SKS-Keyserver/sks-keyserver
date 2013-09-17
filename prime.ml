(***********************************************************************)
(* prime.ml - Generate prime using miller-rabin primality test         *)
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
open Number.Infix

(** returns random string with exactly <bits> bits.  Highest order bit is
  always 1 *)
let randbits rfunc nbits =
  let rval =
    let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
    let rstring = Utils.random_string rfunc nbytes in
    let rand = Number.of_bytes rstring in
    let high = two **! (nbits - 1) in
    high +! (rand %! high)
  in
  assert (Number.nbits rval = nbits);
  rval

(** chooses random int between 0 and high-1 *)
let rec randint rfunc high =
  let nbits = Number.nbits high in
  let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
  let rstring = Utils.random_string rfunc nbytes in
  let rand = Number.of_bytes rstring in
  rand %! high

(** chooses random int between low and high-1 *)
let randrange rfunc low high =
  low +! (randint rfunc (high -! low))

let zerobits n =
  let nbits = Number.nbits n in
  let rec loop count =
    if count >= nbits
    then failwith ("Prime.zerobits: unexpected condition.  " ^
                   "Argument may have been zero");
    if Number.nth_bit n count
    then count
    else loop (count + 1)
  in
  loop 0

let decompose n =
  let s = zerobits n in
  let r = n /! two **! s in
  assert ((two **! s) *! r =! n);
  assert(Number.nth_bit r 0);
  (s,r)

type result = Prime | Composite

let rec test_loop test m =
  if m = 0 then true
  else
    match test () with
        Prime -> test_loop test (m - 1)
      | Composite -> false


(** miller-rabin primality test *)
let miller_rabin rfunc n t =
  let (s,r) = decompose (n -! one) in
  let neg_one = n -! one in

  let test () =
    let a = randrange rfunc two (n -! one) in
    let y = Number.powmod a r n in
    if y =! one || y =! neg_one then Prime
    else
      let rec loop y j =
        if y =! neg_one then Prime
        else if j = s   then Composite
        else
          let y = Number.squaremod y n in
          if y =! one then Composite
          else loop y (j + 1)
      in
      loop y 1

  in
  test_loop test t


let rec randprime rfunc ~bits ~error:t =
  let guess = randbits rfunc bits in
  let guess =  (* force oddness *)
    if guess %! two =! zero
    then guess +! one else guess
  in
  if miller_rabin rfunc guess t
  then guess
  else randprime rfunc ~bits ~error:t


