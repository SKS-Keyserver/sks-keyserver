(***********************************************************************)
(* pdiskTest.ml                                                        *)
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
open Common
open Packet
module Unix = UnixLabels
module PTree = PrefixTree
open Bdb

module Set = PSet.Set

let mbar = !Settings.mbar
let bitquantum = !Settings.bitquantum

let num_samples = mbar + 1
let bytes = ZZp.num_bytes () - 1

(* Generate DB *)
let db_fname = "ptree.db"
let () = if Sys.file_exists db_fname then Unix.unlink db_fname
let db = Db.sopen db_fname Db.BTREE [Db.CREATE] 0o600

let load key = Db.get db key []
let save (txn: unit option) ~key ~data = Db.put db ~key ~data []
let delete (txn: unit option) key = Db.del db key
let dbtup = (load,save,delete,!Settings.max_ptree_nodes)

let db_ptree =
  PTree.create ?db:(Some dbtup) ~txn:None
    ~num_samples ~bitquantum ~thresh:mbar ()

let (ptree:unit PTree.tree) =
  PTree.create ?db:None ~txn:None
     ~num_samples ~bitquantum ~thresh:mbar ()

let set = ref Set.empty

let add_element () =
  let rstring = RMisc.random_string Random.bits bytes in
  set := Set.add rstring !set;
  PTree.insert_str ptree None rstring;
  PTree.insert_str db_ptree None rstring

let del_element () =
  if PTree.size (PTree.root ptree) < 10
  then ()
  else
    let element = PTree.get_random ptree (PTree.root ptree) in
    PTree.delete_str ptree None element;
    PTree.delete_str db_ptree None element;
    set := Set.remove element !set


let node_eq n1 n2 =
  (n1.PTree.svalues = n2.PTree.svalues) &&
  (n1.PTree.num_elements = n2.PTree.num_elements) &&
  (n1.PTree.key = n2.PTree.key) &&
  match (n1.PTree.children,n2.PTree.children) with
      (PTree.Leaf _, PTree.Children _)
    | (PTree.Children _, PTree.Leaf _)  -> false
    | (PTree.Leaf e1,PTree.Leaf e2) -> Set.equal e1 e2
    | (PTree.Children e1, PTree.Children e2) -> true
        (* we don't test the children *)

let sef = true
let rec eqtest (tree1,node1) (tree2,node2) =
  if node_eq node1 node2 then (
    if PTree.is_leaf node1 && PTree.is_leaf node2
    then `passed
    else
      let keys = PTree.child_keys tree1 node1.PTree.key in
      let rec loop keys = match keys with
          [] -> `passed
        | key::tl ->
            let nnode1 = PTree.get_node_key ~sef tree1 key
            and nnode2 = PTree.get_node_key ~sef tree2 key in
            match eqtest (tree1,nnode1) (tree2,nnode2) with
                `passed -> loop tl
              | x -> x
      in
      loop keys
  ) else
    `failed (node1,node2)


let eqtest tree1 tree2 =
  eqtest (tree1, PTree.root tree1) (tree2, PTree.root tree2)

let rec runtest n =
  if n > 0 then (
    if Random.float 1. > !Settings.prob
    then add_element () else del_element ();
    runtest (n - 1)
  ) else (
    printf "-------- Running Equality Test -------------\n";
    match eqtest ptree db_ptree with
        `passed -> printf "All tests passed\n"
      | `failed (n1,n2) ->
          printf "Equality tests failed.  Differing nodes have keys:\n";
          printf "    %s, %s\n"
            (Bitstring.to_string n1.PTree.key)
            (Bitstring.to_string n2.PTree.key)
  )

let n = !Settings.n
let timer = MTimer.create ()
let () =
  if not !Sys.interactive then (
    MTimer.start timer;
    runtest n;
    MTimer.stop timer;
    printf "Time elapsed: %f secs\n" (MTimer.read timer)
  )
