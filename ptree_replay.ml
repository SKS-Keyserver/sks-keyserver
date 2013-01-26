(***********************************************************************)
(* ptree_replay.ml - Test for verifying consistency of prefix tree     *)
(*                   data structure                                    *)
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

open Pstyle
open ReconPTreeDb
open ReconPTreeDb.PDb

(******************************************************************)

let rec get_piece_ch ch s pos =
  if pos >= String.length s then None
  else if s.[pos] = ch then get_piece_ch ch s (pos + 1)
  else
    try
      let nextpos = String.index_from s pos ch in
      Some ((String.sub ~pos ~len:(nextpos - pos) s),nextpos)
    with
        Not_found ->
          Some ((String.sub ~pos ~len:(String.length s - pos) s),
                String.length s)

let rec chsplit ch s pos =
  match get_piece_ch ch s pos with
      None -> []
    | Some (piece,nextpos) -> piece::chsplit ch s nextpos

let chsplit ch s = Array.of_list (chsplit ch s 0)

(******************************************************************)

let hashfile = "log.real"

let rec hashiter ~f file =
  match (try Some (input_line file) with End_of_file -> None)
  with
    | None -> ()
    | Some line ->
        let pieces = chsplit ' ' line in
        let hash = KeyHash.dehexify pieces.(-1) in
        let action = match pieces.(-2) with
          | "Add" -> Add hash
          | "Del" -> Delete hash
          | _ -> failwith "Unexpected action"
        in
        f action;
        hashiter ~f file

let hashiter ~f file =
  ignore (input_line file);
  hashiter ~f file

let apply_action txn action =
  match action with
    | Add hash -> PTree.insert_str !ptree txn hash
    | Delete hash -> PTree.delete_str !ptree txn hash


let () =
  let file = open_in hashfile in
  let txn = new_txnopt () in
  try
    hashiter ~f:(apply_action txn) file;
    commit_txnopt txn;
  with
      e ->
        abort_txnopt txn;
        raise e
