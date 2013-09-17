(***********************************************************************)
(* prefix_test.ml                                                      *)
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

module Set = PSet.Set
open Printf
(*module ZZp = RMisc.ZZp *)
module PTree = PrefixTree

let debug = !Settings.debug

let base = 1000
let bitquantum = !Settings.bitquantum
let num_samples = !Settings.mbar + 1

let (tree: unit option PTree.tree ) =
  PTree.create ~txn:None ~num_samples ~bitquantum ~thresh:!Settings.mbar ()
let timer = MTimer.create ()

let keymatch ~key string =
  let bitlength = Bitstring.num_bits key in
  let bstring = Bitstring.of_bytes_all_nocopy string in
  let keystr = Bitstring.create bitlength in
  Bitstring.blit ~src:bstring ~dst:keystr ~len:bitlength;
  (Bitstring.to_bytes_nocopy keystr) = (Bitstring.to_bytes_nocopy key)

let one = ZZp.of_int 1

let compute_svalue point elements =
  Set.fold
    ~f:(fun el prod -> ZZp.mult prod (ZZp.sub point el))
    ~init:ZZp.one
    elements

let compute_svalues points elements =
  let array =
    Array.map ~f:(fun point -> compute_svalue point elements) points
  in
  ZZp.mut_array_of_array array

let print_vec vec =
  let list = Array.to_list (ZZp.mut_array_to_array vec) in
  MList.print2 ~f:ZZp.print list

(*******************************************************)

let rec add_or_delete setref tree p =
  if Random.float 1. < p
  then (* add element *)
    let zz = ZZp.of_bytes (RMisc.random_string Random.bits !Settings.bytes) in
    PTree.insert tree None zz;
    setref := Set.add zz !setref;
    (*printf "num_elements: counted %d, recorded %d\n"
      (PTree.count_inmem_tree tree) (PTree.get_node_count tree) *)
  else (* remove element *)
    match (try Some (Set.choose !setref) with Not_found -> None) with
        None ->
          printf "*** nothing to delete!\n";
          flush stdout;
          add_or_delete setref tree p
      | Some zz ->
          PTree.delete tree None zz;
          setref := Set.remove zz !setref


(*******************************************************)

exception Notequal

let zza_equal zza1 zza2 =
  let zza1 = ZZp.mut_array_to_array zza1
  and zza2 = ZZp.mut_array_to_array zza2
  in
  if Array.length zza1 != Array.length zza2 then false
  else
    try
      for i = 0 to Array.length zza1 - 1 do
        if ZZp.neq zza1.(i) zza2.(i)
        then raise Notequal
      done;
      true
    with
        Notequal -> false

let () =

  let set = ref Set.empty  in

  for i = 0 to 100000 do
    add_or_delete set tree 0.52
  done;

  let pt_set = PTree.elements tree (PTree.root tree) in
  if Set.equal !set pt_set
  then
    print_string "Set and PTree report identical elements\n"
  else (
    print_string "Failure: Set and PTree report different elements\n";
    printf "Set:  \t%d, %s\n" (Set.cardinal !set) (ZZp.to_string (Set.min_elt !set));
    printf "Tree: \t%d, %s\n" (Set.cardinal pt_set) (ZZp.to_string (Set.min_elt pt_set));
    if Set.subset !set pt_set then
      printf "set is subset of tree\n"
    else if Set.subset pt_set !set then
      printf "tree is susbet of set\n"
    else
      printf "No subset relationship\n"

  );

  if PTree.is_leaf (PTree.root tree)
  then print_string "Root is leaf\n";

  let points = PTree.points tree in

  let rec verify key =
    let node = PTree.get_node_key tree key in
    let elements = PTree.elements tree node in
    let svalues_computed = compute_svalues points elements in
    let svalues = PTree.svalues node in
    if not (zza_equal svalues_computed svalues)
    then (
      print_vec svalues; print_newline ();
      print_vec svalues_computed; print_newline ();
      failwith "svalues do not match";
    );
    let len = Set.cardinal elements
    and reported_len = PTree.size node in
    if not (len = reported_len)
    then ( failwith
             (sprintf "element size %d does not match reported size %d"
                len reported_len ));
    if debug
    then printf "Key: %s,\t num elements: %d\n"
      (Bitstring.to_string key) (Set.cardinal elements);
    Set.iter ~f:(fun el ->
                   if not (keymatch ~key (ZZp.to_bytes el))
                   then failwith "Elements don't match key!") elements;
    let keys = PTree.child_keys tree key in
    if not (PTree.is_leaf node) then
      List.iter ~f:verify keys
  in
  try
    verify (Bitstring.create 0);
    print_string "Verification successful\n";
  with
      Failure s ->
        print_string (sprintf "Verification failed: %s\n" s);




  (*
  MTimer.start timer;
  Array.iteri ~f:(fun i zz -> PTree.insert_str tree zz sa.(i)) zza;
  MTimer.stop timer;

  Printf.printf "Insert time: %f ms,  Depth: %d\n"
    (MTimer.read_ms timer) (PTree.depth tree);
  flush stdout;

  MTimer.start timer;
  let tree = PTree.deepcopy tree in
  MTimer.stop timer;
  Printf.printf "Copy time: %f ms\n" (MTimer.read_ms timer);
  flush stdout;

  let set = ref Set.empty  in
  MTimer.start timer;
  Array.iter ~f:(fun zz -> set := Set.add zz !set) zza;
  MTimer.stop timer;

  Printf.printf "Set Insert time: %f ms\n" (MTimer.read_ms timer);
  flush stdout;
  *)

