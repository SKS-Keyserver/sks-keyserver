(***********************************************************************)
(* server.ml - Server side of set-reconciliation algorithm             *)
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
open Printf

open Common
open ReconMessages

module ZSet = ZZp.Set
module PTree = PrefixTree
(* module ZZp = RMisc.ZZp *)

exception Bug of string

(***************************************************************)
(*  Diagnostic Timers  *****************************************)
(***************************************************************)

let solving = ref 0.0
let lookup = ref 0.0
let flushtime = ref 0.0
let unmarsh_time = ref 0.0

(***************************************************************)
(*  Wrapper for core reconciliation code  *********************)
(***************************************************************)

let solve ~remote_size ~local_size ~remote_samples ~local_samples ~points =
  let values = ZZp.mut_array_div remote_samples local_samples in
  try
    let (remote_diff,local_diff) =
      Decode.reconcile ~values ~points ~d:(remote_size - local_size)
    in
    Some (remote_diff,local_diff)
  with
      Decode.Low_mbar -> None

(************************************************)


(* returns true if the connection should be left open, false otherwise *)
let handle_one tree cin cout =
  let request = unmarshal cin in
  match request.msg with

    | Elements s ->
        (true, s)

    | ReconRqst_Poly rp ->  (
        (* NOTE: Add case analysis to deal with where set size = 0 *)
        let remote_size = rp.rp_size
        and points = PTree.points tree
        and remote_samples = rp.rp_samples in
        ( match (try Some (PTree.get_node_key tree rp.rp_prefix)
                 with Not_found -> None)
          with
              None ->
                marshal cout
                (Error("server should never receive request " ^
                       "for non-existant node (ReconRqst_Poly)"));
                plerror 2 "%s" ("Server received ReconRqst_Poly " ^
                                         "for non-existant node");
                (false,ZSet.empty)
            | Some node ->
                let local_samples = PTree.svalues node
                and local_size = PTree.size node in
                let results =
                  solve ~remote_samples ~local_samples ~remote_size
                    ~local_size ~points  in
                match results with
                  | Some (remote_set,local_set) ->
                      marshal_noflush cout (Elements local_set);
                      (true,remote_set)
                  | None ->
                      if PTree.is_leaf node ||
                        PTree.num_elements tree node <
                        !Settings.recon_thresh_mult * !Settings.mbar
                      then (
                        let elements = PTree.elements tree node in
                        marshal_noflush cout (FullElements elements);
                        (true,ZSet.empty)
                        (* NOTE: server still doesn't know its share here.
                           Client will send that later *)
                      ) else (
                        marshal_noflush cout SyncFail;
                        (true, ZSet.empty)
                      )

        ))

    | ReconRqst_Full rf ->  (
        match
          ( try
              let node = PTree.get_node_key tree rf.rf_prefix in
              let localset = PTree.elements tree node in
              Some (ZSet.diff localset rf.rf_elements,
                    ZSet.diff rf.rf_elements localset)
            with
                Not_found -> None )
        with
            Some (localdiff,remotediff) ->
              marshal_noflush cout (Elements localdiff);
              (true, remotediff)
          | None ->
              marshal cout (Error ("server should never received request " ^
                                   "for non-existant node (ReconRqst_Full)"));
              plerror 2 "%s" ("Server recieved RconRqst_Full " ^
                              "for non-existant node");
              (false,ZSet.empty)
      )

    | Done ->
        plerror 5 "Done received";
        (false,ZSet.empty)

    | Flush ->
        plerror 5 "Flush occured";
        cout#flush;
        (true,ZSet.empty)

    | _ ->
        failwith ("Unexpected message: " ^
                  msg_to_string request.msg)

(***************************************************************)


let recover_timeout = 10

let handle tree cin cout =
  let set_ref = ref ZSet.empty in
  let continue_ref = ref true in
  try
    while !continue_ref do
      let (continue, elements) = handle_one tree cin cout in
      set_ref := ZSet.union !set_ref elements;
      continue_ref := continue;
    done;
    !set_ref
  with
    | Eventloop.SigAlarm ->
        ignore (Unix.alarm recover_timeout);
        plerror 2 "%s" ("Reconciliation failed due to timeout.  " ^
                        "Returning elements returned so far");
        !set_ref
    | End_of_file | Sys_error _ as e ->
        ignore (Unix.alarm recover_timeout);
        eplerror 2 e "%s" ("Reconciliation failed.  " ^
                           "Returning elements returned so far");
        !set_ref


