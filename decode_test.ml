(***********************************************************************)
(* decode_test.ml - Unit tests for number.ml                           *)
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
open Printf
open Decode
open Common
open ZZp.Infix
module ZSet = ZZp.Set

let rand_int = Random.State.int RMisc.det_rng
let rand_bits () = Random.State.bits RMisc.det_rng

(*************************************************************************)
(** Simple counter table *)

let ctr_table = Hashtbl.create 0

let incr_count name =
  try
    let ctr_ref = Hashtbl.find ctr_table name in
    incr ctr_ref
  with
      Not_found ->
        Hashtbl.add ctr_table ~key:name ~data:(ref 1)

let read_count name =
  try !(Hashtbl.find ctr_table name)
  with Not_found -> 0

(*************************************************************************)

let test name cond =
  printf ".%!";
  incr_count name;
  if not cond then raise
    (Unit_test_failure (sprintf "Decode test <%s:%d> failed"
                          name (read_count name)))

(** creates a random monic polynomial of desired dimension *)
let rand_poly dim =
  let poly = Array.init (dim + 1)
               ~f:(fun i ->
                     if i = dim then ZZp.one
                     else ZZp.rand rand_bits)
  in
  Poly.of_array poly

let interp_test () =
  let deg = rand_int 10 + 1 in
  let num_deg = rand_int deg in
  let denom_deg = deg - num_deg in
  let num = rand_poly num_deg in
  let denom = rand_poly denom_deg in
  test "poly construction"
    (Poly.degree num == num_deg && Poly.degree denom = denom_deg );

  let mbar = rand_int 9 + 1 in
  let n = mbar + 1 in

  let toobig = deg + 1 > mbar in
  let values  = ZZp.mut_array_to_array (ZZp.svalues n) in
  let points = ZZp.points n in
  for i = 0 to Array.length values - 1 do
    values.(i) <- Poly.eval num points.(i) /: Poly.eval denom points.(i)
  done;
  try
    let (found_num,found_denom) =
      Decode.interpolate ~values ~points ~d:(num_deg - denom_deg)
    in
(*    printf "mbar: %d, num_deg: %d, denom_deg: %d\n" mbar num_deg denom_deg;
    printf "num: %s\ndenom: %s\n%!" (Poly.to_string num) (Poly.to_string denom);
    printf "gcd: %s\n" (Poly.to_string (Poly.gcd num denom));
    printf "found num: %s\nfound denom: %s\n%!"
      (Poly.to_string found_num) (Poly.to_string found_denom); *)
    test "degree equality" (toobig
                            || (Poly.degree found_num = Poly.degree num
                                && Poly.degree found_denom = Poly.degree denom));
    test "num equality" (toobig || Poly.eq found_num num);
    test "denom equality" (toobig || Poly.eq found_denom denom);
  with
      Interpolation_failure ->
        test (sprintf "interpolation failed (deg:%d,mbar:%d)" deg mbar)
          (deg + 1 > mbar)


let set_init ~f n =
  let rec loop n set =
    if n = 0 then set
    else loop (n - 1) (ZSet.add (f ()) set)
  in
  loop n ZSet.empty

let ( &> ) f g x = f (g x)
let ( &< ) g f x = f (g x)
let ( @@ ) f x = f x

(** Test full reconciliation, from beginning to end *)
let reconcile_test () =
  let mbar = rand_int 20 + 1 in (* maximum recoverable # of points *)
  let n = mbar + 1 in (* Number of sample values to capture *)
  let points = ZZp.points n in (* Array of evaluation points *)
  let svalues1 = ZZp.svalues n in (* sample values 1 *)
  let svalues2 = ZZp.svalues n in (* sample values 2 *)
  let m = rand_int (mbar * 2) + 1 in (* diff size to be reconciled *)
  (* m1 and m2 are a partitioning of m *)
  let m1 = rand_int m in
  let m2 = m - m1 in
  let set1 = set_init m1 ~f:(fun () -> ZZp.rand rand_bits) in
  let set2 = set_init m2 ~f:(fun () -> ZZp.rand rand_bits) in
  (* printf "mbar: %d, m: %d, m1: %d, m2: %d\n%!" mbar m m1 m2; *)
  test "full sets" (ZSet.cardinal set1 = m1 && ZSet.cardinal set2 = m2);
  test "empty intersection" (ZSet.is_empty @@ ZSet.inter set1 set2);
  ZSet.iter ~f:(fun x -> ZZp.add_el ~svalues:svalues1 ~points x) set1;
  ZSet.iter ~f:(fun x -> ZZp.add_el ~svalues:svalues2 ~points x) set2;
  let values = ZZp.mut_array_div svalues1 svalues2 in
  try
    let (diff1,diff2) =
      Decode.reconcile ~values ~points ~d:(m1 - m2)
    in
    test "size equality set1"
      (ZSet.cardinal set1 = ZSet.cardinal diff1);
    test "size equality set2"
      (ZSet.cardinal set2 = ZSet.cardinal diff2);
    test "recon compare" (ZSet.equal diff1 set1 && ZSet.equal diff2 set2)
  with
      Low_mbar -> test "low mbar" (m > mbar)

let factorization_test () =
  let deg = rand_int 10 + 1 in
  let terms = Array.to_list (Array.init deg (fun _ -> rand_poly 1)) in
  let poly = List.fold_left ~init:Poly.one ~f:Poly.mult terms in
  let roots = Decode.factor poly in
  let orig_roots =
    ZZp.zset_of_list (List.map ~f:(fun p -> ZZp.neg (Poly.to_array p).(0)) terms)
  in
  test "factor equality" (ZSet.equal orig_roots roots)

let interp_run () =
  let deg = rand_int 10 + 1 in
  let num_deg = rand_int deg in
  let denom_deg = deg - num_deg in
  let num = rand_poly num_deg in
  let denom = rand_poly denom_deg in
  if not (Poly.degree num == num_deg && Poly.degree denom = denom_deg )
  then `poly_gen_falure (deg,num_deg,denom_deg,num,denom)
  else

    let mbar = rand_int 9 + 1 in
    let n = mbar + 1 in

    let values  = ZZp.mut_array_to_array (ZZp.svalues n) in
    let points = ZZp.points n in
    for i = 0 to Array.length values - 1 do
      values.(i) <- Poly.eval num points.(i) /: Poly.eval denom points.(i)
    done;
    try
      let (found_num,found_denom) =
        Decode.interpolate ~values ~points ~d:(num_deg - denom_deg)
      in
      `succ ((num,denom),(found_num,found_denom),mbar)
    with
        Interpolation_failure ->
          `fail ((num,denom),mbar)


let run () =
  begin
    for i = 1 to 100 do factorization_test () done;
    for i = 1 to 100 do interp_test () done;
    for i = 1 to 100 do reconcile_test () done;
  end
