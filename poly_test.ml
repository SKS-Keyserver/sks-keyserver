(***********************************************************************)
(* poly_test.ml - unit tests for Poly module                           *)
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

open Common
open StdLabels
open MoreLabels
module Unix = UnixLabels
open Printf
open ZZp.Infix


let rand_int n = Random.State.int RMisc.det_rng n
let rand_bits () = Random.State.bits RMisc.det_rng

let ctr = ref 0
let test name cond =
  printf ".%!";
  incr ctr;
  if not cond then raise
    (Unit_test_failure (sprintf "Poly test %s:%d failed" name !ctr))


let divtest () =
  let x = Poly.of_array [| ZZp.one; ZZp.one; ZZp.one; ZZp.one |] in
  let c = ZZp.of_int 5 in
  let y = Poly.of_array [| c; c; c |] in
  let (q,r) = Poly.divmod x y in
  test "invtest" (Poly.eq x (Poly.add (Poly.mult y q) r));
  test "rtest" (Poly.eq r (Poly.of_array [| ZZp.one |]));
  test "qtest" (Poly.eq q (Poly.of_array [| ZZp.zero; ZZp.inv c |]))

let rand_divtest () =
  let p1 = Poly.of_array (Array.init (1 + rand_int 20)
                            ~f:(fun i -> ZZp.rand rand_bits)) in
  let p2 = Poly.of_array (Array.init (1 + rand_int 20)
                            ~f:(fun i -> ZZp.rand rand_bits)) in
  let (q,r) = Poly.divmod p1 p2 in
  let z = ZZp.rand rand_bits in
  let r_z = Poly.eval r z
  and q_z = Poly.eval q z
  and p1_z = Poly.eval p1 z
  and p2_z = Poly.eval p2 z
  in
  test "rand_divtest" (p1_z =: p2_z *: q_z +: r_z)

(** returns true iff y divides x *)
let divides x y =
  Poly.eq (Poly.modulo x y) Poly.zero

let gcd_test () =
  let p1 = Poly.of_array (Array.init (1 + rand_int 20)
                            ~f:(fun i -> ZZp.rand rand_bits)) in
  let p2 = Poly.of_array (Array.init (1 + rand_int 20)
                            ~f:(fun i -> ZZp.rand rand_bits)) in
  let p3 = Poly.of_array (Array.init (1 + rand_int 20)
                            ~f:(fun i -> ZZp.rand rand_bits)) in
  let p1 = Poly.mult p1 p3 in
  let p2 = Poly.mult p2 p3 in
  let gcd = Poly.gcd p1 p2 in
  test "gcd - p3 div" (divides gcd p3);
  test "gcd - gcd div 1" (divides p1 gcd);
  test "gcd - gcd div 2" (divides p2 gcd);
  let p1 = Poly.div p1 gcd in
  let p2 = Poly.div p2 gcd in
  let gcd = Poly.gcd p1 p2 in
  test "gcd - zero" (Poly.degree gcd = 0)


let run () =
  begin
    for i = 1 to 100  do
      rand_divtest ()
    done;
    for i = 1 to 100  do
      gcd_test ()
    done;
    divtest ();
  end
