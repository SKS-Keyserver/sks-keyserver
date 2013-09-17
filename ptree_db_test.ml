(***********************************************************************)
(* ptree_db_test.ml - Checks whether the memory-bounds on a ptree are  *)
(*                    in force. Test for verifying consistency of      *)
(*                    prefix tree data structure.                      *)
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

(* #directory "/home/yminsky/Work/projects/keyserver/sks";;
let () = Sys.chdir "/usr/share/keyfiles/sks_the";;
#load "reconPTreeDb.cmo";;
*)

open Printf
open StdLabels
open MoreLabels
module Set = PSet.Set

open Common

open ReconPTreeDb
open ReconPTreeDb.PDb

let root = (!ptree).PTree.root

let random_probe () =
  let zzs = PTree.get_random !ptree root in
  let depth = ref 0 in
  while
    let node = PTree.get_node_str !ptree zzs !depth in
    if PTree.is_leaf node then false
    else true
  do incr depth done



let inmem_count () =
  match !ptree.PTree.db with
      None -> failwith "DB expected"
    | Some db -> db.PTree.inmem_count
