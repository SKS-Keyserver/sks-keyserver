(***********************************************************************)
(* ptest.ml                                                            *)
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
open Bdb

module Set = PSet.Set

module Keydb =
  Keydb.Make(struct
               let withtxn = !Settings.transactions
               and cache_bytes = !Settings.cache_bytes
               and pagesize = !Settings.pagesize
               and dbdir = !Settings.dbdir
               and dumpdir = !Settings.dumpdir
             end)

module PTreeDB =
  PTreeDB.Make(struct
                 let mbar = !Settings.mbar
                 and bitquantum = !Settings.bitquantum
                 and treetype = `ondisk
                 and max_nodes = !Settings.max_ptree_nodes
                 and dbdir = !Settings.ptree_dbdir
                 and cache_bytes = !Settings.ptree_cache_bytes
                 and pagesize = !Settings.ptree_pagesize
               end)
open PTreeDB

module PTree = PrefixTree

let () = PTreeDB.init ()
let () = Keydb.open_dbs ()
let ptree = PrefixTree.create ?db:(get_db ()) ~txn:None
              ~num_samples ~bitquantum
              ~thresh:(mbar * !Settings.ptree_thresh_mult) ()

let trunc s = String.sub ~pos:0 ~len:16 s


let i = ref 0
let get_ptree_hashes () =
  PTree.summarize_tree
    ~lagg:(fun set -> Array.map ~f:trunc
             (Array.of_list (Set.elements set)))
    ~cagg:(fun alist -> Array.concat (Array.to_list alist))
    ptree


let sstream_array_get size stream =
  match SStream.peek stream with
      None -> [| |]
    | Some first ->
        let array = Array.make size first in
        let ctr = ref 0 in
        let emptystream = ref false in
        while (!ctr < Array.length array &&
               not !emptystream )
        do
          match SStream.next stream with
              Some hash ->
                array.(!ctr) <- hash;
                incr ctr
            | None ->
                emptystream := true
        done;
        if !ctr <> Array.length array then
          Array.sub ~pos:0 ~len:!ctr array
        else
          array

let get_kdb_hashes () =
  let chunksize = 5000 in
  let (stream,close) = Keydb.create_hashstream () in
  let rec loop alist =
    let newarray = sstream_array_get chunksize stream in
    if newarray = [| |] then
      List.rev alist
    else
      loop (newarray::alist)
  in
  let alist = loop [] in
  let array = Array.concat alist in
  array


let is_sorted ~cmp array =
  let rec loop i =
    if i >= Array.length array - 1 then
      true
    else (
      if cmp array.(i+1)  array.(i) > 0 then loop (i+1)
      else false
    )
  in
  loop 0

(** compute the symmetric difference between two arrays
  sorted in increasing order
*)
let array_diff a1 a2 =
  let c1 = ref 0 and c2 = ref 0 in
  let diff1 = ref [] and diff2 = ref [] in

  let add1 () =
    diff1 := a1.(!c1)::!diff1;
    incr c1
  and add2 () =
    diff2 := a2.(!c2)::!diff2;
    incr c2
  in

  while !c1 < Array.length a1 || !c2 < Array.length a2 do
    if !c1 >= Array.length a1 then add2 ()
    else if !c2 >= Array.length a2 then add1 ()
    else if a1.(!c1) = a2.(!c2) then ( incr c1; incr c2; )
    else if a1.(!c1) < a2.(!c2) then add1 ()
    else add2 ()
  done;
  (List.rev !diff1,List.rev !diff2)


let () =
  if not !Sys.interactive then
    perror "Getting Keydb hashes";
    let khashes = get_kdb_hashes () in
    perror "Getting PTree hashes";
    let phashes = get_ptree_hashes () in
    perror "Comparing hashes";
    let (diff1,diff2) = array_diff phashes khashes in
    let (diff1,diff2) = (List.map ~f:KeyHash.hexify diff1,
                         List.map ~f:KeyHash.hexify diff2)
    in
    printf "Prefix side:\n";
    MList.print2 ~f:(printf "%s") diff1;
    printf "\n\nKeydb side:\n";
    MList.print2 ~f:(printf "%s") diff2;
    printf "\n"

let () =
  perror "Closing DBs";
  Keydb.close_dbs ();
  PTreeDB.closedb ()
