(***********************************************************************)
(* update_subkeys.ml                                                   *)
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
open Arg
open Common
module Set = PSet.Set
module Map = PMap.Map
module Unix = UnixLabels
open Packet
open Bdb

let settings = {
  Keydb.withtxn = !Settings.transactions;
  Keydb.cache_bytes = !Settings.cache_bytes;
  Keydb.pagesize = !Settings.pagesize;
  Keydb.keyid_pagesize = !Settings.keyid_pagesize;
  Keydb.meta_pagesize = !Settings.meta_pagesize;
  Keydb.subkeyid_pagesize = !Settings.subkeyid_pagesize;
  Keydb.time_pagesize = !Settings.time_pagesize;
  Keydb.tqueue_pagesize = !Settings.tqueue_pagesize;
  Keydb.word_pagesize = !Settings.word_pagesize;
  Keydb.dbdir = Lazy.force Settings.dbdir;
  Keydb.dumpdir = Lazy.force Settings.dumpdir;
}

(** we need full keydb access because we're playing directly with
  databases and cursors and such
*)
module Keydb = Keydb.Unsafe

type update = { keyid: string;
                hash: string;
              }

let ( |= ) map key = Map.find key map
let ( |< ) map (key,data) = Map.add ~key ~data map

let at_once = match !Settings.n with
    0 -> 10000
  | n -> n * 1000

let subkeyids_from_key key =
  let (keyid,subkey_keyids) = Fingerprint.keyids_from_key ~short:true key in
  subkey_keyids

(** returns a copy of the list without duplicates in sorted order *)
let sort_dedup list =
  let list = List.sort ~cmp:(fun x y -> compare y x) list in
  let rec dedup list partial = match list with
    | [] -> partial
    | hd::[] -> dedup [] (hd::partial)
    | hd1::hd2::tl ->
        if hd1 = hd2 then dedup (hd2::tl) partial
        else dedup (hd2::tl) (hd1::partial)
  in
  dedup list []


(** takes a list of updates and applies them to the database *)
let apply_updates updates =
  let dbs = Keydb.get_dbs () in
  perror "%d updates found.  Applying to database" (List.length updates);
  let updates = sort_dedup updates  in
  let txn = Keydb.txn_begin () in
  try
    List.iter ~f:(fun update ->
                    try Db.put ?txn dbs.Keydb.subkey_keyid ~key:update.keyid
                      ~data:update.hash [Db.NODUPDATA]
                    with
                        Key_exists -> ()
                 )
      updates;
    Keydb.txn_commit txn;
    perror "Application of updates complete."
  with
    | Bdb.DBError s as e ->
        eplerror 0 e "Fatal database error";
        raise Sys.Break
    | e ->
        eplerror 1 e "apply_md_updates failed -- aborting txn";
        Keydb.txn_abort txn;
        raise e

(** iterate through the database, extracting updates that need to be
  applied and applies them *)
let fix_keyids () =
  perror "Beginning subkeyid update process";
  let updates = ref [] in
  let ctr = ref 0 in

  let process_key ~hash ~key =
    let subkeyids = subkeyids_from_key key in
    let new_updates =
      List.map subkeyids
        ~f:(fun subkeyid -> { keyid = subkeyid; hash = hash })
    in
    updates := List.rev_append new_updates !updates;
    ctr := !ctr + List.length new_updates;
    if !ctr >= at_once then (
      apply_updates !updates;
      ctr := 0;
      updates := []
    )
  in
  Keydb.iter process_key;
  (* need one more call to apply_updates to add the final batch *)
  apply_updates !updates

let run () =
  set_logfile "update_subkeys";
  perror "Running SKS %s%s" Common.version Common.version_suffix;
  Keydb.open_dbs settings;
  perror "Keydb opened";

  fix_keyids ();
  perror "Subkey update complete. Checkpointing database.";
  Keydb.checkpoint ();
  perror "Checkpoint complete.  Closing.";
  Keydb.close_dbs ();
  perror "Database closed.";

