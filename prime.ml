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

open StdLabels
open MoreLabels
module Unix=UnixLabels

(* Generate prime using miller-rabin primality test *)

module Nx = Number.Nx

(* returns random string with exactly <bits> bits.  
   Highest order bit is always 1 *)
let randbits rfunc nbits =  
  let rval = 
    let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
    let rstring = Utils.random_string rfunc nbytes in
    let rand = Number.of_bytes rstring in
    let high = Nx.shl Number.one (nbits - 1) in
    Nx.add high  (Nx.modulo rand high)
  in
  assert (Nx.nbits rval = nbits);
  rval

(* chooses random int between 0 and high-1 *)
let rec randint rfunc high = 
  let nbits = Nx.nbits high in
  let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
  let rstring = Utils.random_string rfunc nbytes in
  let rand = Number.of_bytes rstring in
  Nx.modulo rand high

(* chooses random int between low and high-1 *)
let randrange rfunc low high = 
  Nx.add low (randint rfunc (Nx.sub high low))

let zerobits n =
  let nbits = Nx.nbits n in
  let rec loop count = 
    if count >= nbits 
    then failwith ("Prime.zerobits: unexpected condition.  " ^
		   "Argument may have been zero");
    if Nx.nth_bit n count 
    then count
    else loop (count + 1)
  in
  loop 0

let decompose n = 
  let s = zerobits n in
  let r = Nx.shr n s in
  assert ((Nx.mul (Nx.pow Number.two s) r = n) &&
	  (Nx.nth_bit r 0));
  (s,r)


type result = Prime | Composite

let rec test_loop test m = 
  if m = 0 then true
  else
    match test () with
	Prime -> test_loop test (m - 1)
      | Composite -> false


(* miller-rabin primality test *)
let miller_rabin rfunc n t = 
  let (s,r) = decompose (Nx.sub n Number.one) in
  let neg_one = Nx.sub n Number.one in

  let test () = 
    let a = randrange rfunc Number.two (Nx.sub n Number.one) in
    let y = Nx.powmod a r n in
    if y = Number.one or y = neg_one then Prime
    else
      let rec loop y j =
	if y = neg_one then Prime
	else if j = s   then Composite
	else
	  let y = Nx.modulo (Nx.mul y y) n in
	  if y = Number.one then Composite
	  else loop y (j + 1)
      in
      loop y 1

  in 
  test_loop test t


let rec randprime rfunc ~bits ~error:t = 
  let guess = randbits rfunc bits in
  let guess =  (* force oddness *)
    if Nx.modulo guess Number.two = Number.zero then Nx.add guess Number.one else guess in
  if miller_rabin rfunc guess t
  then guess
  else randprime rfunc ~bits ~error:t
  

