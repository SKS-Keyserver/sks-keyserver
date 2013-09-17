(***********************************************************************)
(* ptree_consistency_test.ml - Test for verifying consistency of       *)
(*                             prefix tree data structure              *)
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
module Set = PSet.Set

open ReconPTreeDb

let ident x = x

let node_to_svalues node = node.PTree.svalues

let check_svalues parent children =
  let parent = ZZp.zzarray_to_array parent in
  let children = List.map ~f:ZZp.zzarray_to_array children in
  match children with
      [] -> failwith "check_svalues: no children to check"
    | hd::tl ->
        parent = List.fold_left ~f:ZZp.array_mult ~init:hd tl

let check_node ptree parent children =
  check_svalues parent.PTree.svalues
    (List.map ~f:node_to_svalues children)

let check_leaf ptree node =
  let points = ptree.PTree.points in
  let svalues = PTree.create_svalues points in
  match node.PTree.children with
    | PTree.Children _ -> failwith "check_leaf called on non-leaf node"
    | PTree.Leaf children ->
        Set.iter children ~f:(fun zzs ->
                                let zz = ZZp.of_bytes zzs in
                                ZZp.add_el ~svalues ~points zz
                             );
        (ZZp.zzarray_to_array node.PTree.svalues =
           ZZp.zzarray_to_array svalues)

let rec check_tree ptree node =
  let key = node.PTree.key in
  let keyrep = Bitstring.to_string key in
  if PTree.is_leaf node then
    let rval = check_leaf ptree node in
    if rval
    then perror "leaf passed: %s" keyrep
    else perror "leaf failed: %s" keyrep;
    rval
  else
    let childkeys = PTree.child_keys ptree key in
    let children =
      List.map ~f:(fun key -> PTree.get_node_key ptree key) childkeys
    in
    let node_passed = check_node ptree node children in
    if node_passed
    then perror "internal node passed: %s" keyrep
    else perror "internal node failed: %s" keyrep;
    let child_status = List.map ~f:(check_tree ptree) children in
    node_passed &
    List.for_all ~f:ident child_status

let () =
  perror "Starting recursive check";
  if check_tree !ptree (!ptree).PTree.root
  then perror "tree passed"
  else perror "tree FAILED"
