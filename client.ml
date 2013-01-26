(***********************************************************************)
(* client.ml - Client side of set-reconciliation algorithm             *)
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
open Common

open Printf
open ReconMessages

module Set = PSet.Set
module Map = PMap.Map
module PTree = PrefixTree
(* module ZZp = RMisc.ZZp *)

exception Bug of string

(***************************************************************)
(*  Diagnostic Timers  *****************************************)
(***************************************************************)

let flushcount = ref 0

let timer = MTimer.create ()
let tstart () =
  MTimer.start timer
let tstop accum =
  MTimer.stop timer;
  accum := !accum +. MTimer.read_ms timer

let get_flushcount () = !flushcount

(***************************************************************)
(***************************************************************)
(***************************************************************)

type 'a bottomQ_entry = FlushEnded | Bottom of 'a
type reconbound = { num_completed: int;
                    verified_partitions: Bitstring.t Set.t;
                  }


(*
let reconbound_exceeded rb =
  !Settings.mbar * (Set.cardinal rb.verified_partitions)
  + rb.num_recovered
  > Settings.max_recover
*)

exception Continue

(** Send request and update [bottomQ] appropriately *)
let send_request cout tree ~bottomQ (node,key) =
  let request =
    if PTree.is_leaf node ||
      PTree.num_elements tree node <
      !Settings.recon_thresh_mult * !Settings.mbar
    then ReconRqst_Full
      { rf_prefix = key;
        rf_elements = PTree.elements tree node;
      }
    else ReconRqst_Poly
      { rp_prefix = key;
        rp_size = PTree.size node;
        rp_samples = PTree.svalues node;
      }
  in
  marshal_noflush cout request;
  Queue.push (Bottom (node,key)) bottomQ

(** Handle reply message and update [requestQ] appropriately *)
let handle_reply cout tree ~requestQ reply (node,key) setref =
  match reply.msg with
    | SyncFail ->
        if PTree.is_leaf node then
           raise (Bug ("Unexpected error.  Syncfail received" ^
                       "at leaf node"));
        let children = PTree.child_keys tree key in
        let nodes =
          List.map
             ~f:(fun key -> try PTree.get_node_key tree key
                with Not_found ->
                   raise (Bug ("Client.read: PTree.get_node_key " ^
                               "should not fail")))
             children in
        (* update requestQ with requests corresponding to
           children of present node *)
        List.iter  ~f:(fun req -> Queue.push req requestQ)
          (List.combine nodes children)

    | Elements elements -> setref := (ZZp.Set.union !setref elements)

    (* required for case where reconciliation terminates for due to the end
       of the prefix tree *)
    | FullElements elements ->
        let local = PTree.get_zzp_elements tree node in
        let localdiff = ZZp.Set.diff local elements in
        let remotediff = ZZp.Set.diff elements local in
        marshal_noflush cout (Elements localdiff);
        setref := ZZp.Set.union !setref remotediff

    | _ -> failwith ( "Unexpected message: " ^
                       msg_to_string reply.msg )


(* after a timeout, give an extra 10 seconds to actually extract the data built up so far *)
let recover_timeout = 10

(** manages reconciliation connection, determining when messages are sent and
  received on the channel. *)
let connection_manager cin cout tree initial_request =
  let set = ref ZZp.Set.empty in
  let requestQ = Queue.create ()
  and bottomQ = Queue.create () in

  Queue.push initial_request requestQ;

  (* state variables *)
  let flushing = ref false (* whether a flush has been sent and not
                              yet bounced back. *)
  in

  let flush_queue () =
    marshal_noflush cout Flush;
    cout#flush;
    Queue.push FlushEnded bottomQ;
    flushing := true
  in


  try
    (* Once both queues are empty, the reconciliation is done *)
    while not (Queue.is_empty requestQ && Queue.is_empty bottomQ) do
      match (try Some (Queue.top bottomQ) with Queue.Empty -> None) with
        | None ->
            (* following pop is safe, because requestQ can't be empty *)
            let (node,key) = Queue.pop requestQ in
            send_request cout tree ~bottomQ (node,key)
        | Some FlushEnded ->
            ignore (Queue.pop bottomQ);
            flushing := false
        | Some (Bottom (node,key)) ->
            plerror 10 "Queue length: %d" (Queue.length bottomQ);
            match try_unmarshal cin with
              | Some reply ->
                  ignore (Queue.pop bottomQ);
                  handle_reply cout tree ~requestQ reply (node,key) set
              | None ->
                  match (
                    if Queue.length bottomQ > !Settings.max_outstanding_recon_requests
                    then None
                    else
                      try Some (Queue.pop requestQ)
                      with Queue.Empty -> None
                  )
                  with
                    | None ->
                        if not !flushing then flush_queue ()
                        else (
                          ignore (Queue.pop bottomQ);
                          let reply = unmarshal cin in
                          handle_reply cout tree ~requestQ reply (node,key) set
                        )
                    | Some (node,key) ->
                        send_request cout tree ~bottomQ (node,key)
    done;
    marshal cout Done;
    !set
  with
    | Eventloop.SigAlarm ->
        ignore (Unix.alarm recover_timeout);
        plerror 2 "%s" ("Reconciliation failed due to timeout.  " ^
                        "Returning elements returned so far");
        !set
    | End_of_file | Sys_error _ as e ->
        ignore (Unix.alarm recover_timeout);
        eplerror 2 e "%s" ("Reconciliation failed.  " ^
                           "Returning elements returned so far");
        !set


(* Main reconciliation code *)
let handle tree cin cout =
  flushcount := 0; (* number of round-trips *)
  let startkey = Bitstring.create 0 in
  connection_manager cin cout tree (PTree.root tree, startkey)
