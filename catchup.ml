(***********************************************************************)
(* catchup.ml - code used by the reconserver to catch up on whatever   *)
(*              updates have been made to the key database             *)
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
open DbMessages
open PTreeDB

(***************************************************************)
(*  Catchup Code   *********************************************)
(***************************************************************)

let rec last_ts log = match log with
    [] -> raise Not_found
  | (ts,event)::[] -> ts
  | hd::tl -> last_ts tl

let event_to_hash event = match event with
  | Add hash -> hash
  | Delete hash -> hash

(** sort log in hash order, respecting ordering of adds/deletes
  within a single hash
*)
let sortlog log =
  List.stable_sort log
    ~cmp:(fun (_,ev1) (_,ev2) ->
            compare (event_to_hash ev1) (event_to_hash ev2)
         )

let rec applylog txn log = match log with
    [] -> ()
  | (ts,Add hash)::tl ->
      PTree.insert_str (get_ptree ()) txn hash;
      applylog txn tl
  | (ts,Delete hash)::tl ->
      PTree.delete_str (get_ptree ()) txn hash;
      applylog txn tl


let combine ~f list = match list with
    [] -> failwith "combine needs at least one element"
  | first::rest -> List.fold_left ~init:first ~f rest

let max_timestamp log = combine ~f:max (List.map ~f:fst log)

let applylog txn log =
  applylog txn (sortlog log);
  let ts = max_timestamp log in
  plerror 5 "setting synctime to %f" ts;
  PTree.set_synctime (get_ptree ()) ts

(** does a single catchup-run, returning true if no results were retrieved
  by the catchup *)
let single_catchup count =
  let resp = ReconComm.send_dbmsg
               (LogQuery (count,PTree.get_synctime (get_ptree ()))) in
  let log =
    match resp with
      | LogResp log -> log
      | _ -> failwith "Unexpected response"
  in
  match log with
    | [] -> true
    | _ ->
        let length = List.length log in
        let newts = last_ts log in
        let old_timeout = Unix.alarm 0 in
        Eventloop.waiting_for_alarm := false;
        let txn = new_txnopt () in
        begin
          try
            applylog txn log;
            plerror (if length = 0 then 5 else 3)
              "Added %d hash-updates. Caught up to %f"
              length newts;
            PTree.clean txn (get_ptree ());
            commit_txnopt txn
          with
            | Sys.Break ->
                abort_txnopt txn;
                raise Sys.Break
            | e ->
                eplerror 1 e
                  "Raising Sys.Break -- PTree may be corrupted";
                abort_txnopt txn;
                raise Sys.Break
        end;
        Eventloop.waiting_for_alarm := true;
        ignore (Unix.alarm old_timeout);
        false


let count = 5000

let rec uninterruptable_catchup () =
  if single_catchup count
  then ()
  else uninterruptable_catchup ()

let rec catchup () =
  if single_catchup count
  then []
  else
    let now = Unix.gettimeofday () in
    [ Eventloop.Event
        (now,
         Eventloop.make_tc ~name:"further catchup"
           ~timeout:max_int ~cb:catchup
        )
    ]

let catchup_interval = 5.

