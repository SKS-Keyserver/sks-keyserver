(***********************************************************************)
(* number_test.ml                                                      *)
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
open Number
open Number.Infix
open Common

(** Unit tests for number.ml *)

let rand_int = Random.State.int RMisc.det_rng
let rand_bits () = Random.State.bits RMisc.det_rng

let ctr = ref 0
let test cond =
  printf ".%!";
  incr ctr;
  if not cond then raise (Unit_test_failure (sprintf "Number test %d failed" !ctr))


let conversion_test () =
  let nbits = rand_int 400 + 1 in
  let nbytes = nbits / 8 + (if nbits mod 8 = 0 then 0 else 1) in
  let x = Prime.randbits rand_bits nbits in
  let xstr = to_bytes ~nbytes x in
  test (of_bytes xstr =! x)

let powmod_test () =
  let x = Prime.randbits rand_bits (rand_int 12 + 1) in
  let y = Prime.randbits rand_bits (rand_int 12 + 1) in
  let m = Prime.randbits rand_bits (rand_int 12 + 1) in
  test (powmod x y m =! dumb_powmod x y m)


let run () =
  for i = 1 to 100 do conversion_test () done;
  for i = 1 to 100 do powmod_test () done;
