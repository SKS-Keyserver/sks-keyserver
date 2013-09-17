(***********************************************************************)
(* query.ml - Executable: Simple tool for direct querying key db.      *)
(*            Should not be used while dbserver is running             *)
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
open Packet

module Keydb = Keydb.Make(struct
                            let withtxn = false
                            and cache_bytes = !Settings.cache_bytes
                            and pagesize = !Settings.pagesize
                            and dbdir = !Settings.dbdir
                            and dumpdir = !Settings.dumpdir
                          end)


let dbdir = !Settings.dbdir

let _ =
  Keydb.open_dbs ()

let _ =
  try
    while true do
      let line = try read_line () with End_of_file -> raise Exit in
      try
        let words = Keydb.extract_words line in

        print_string "   Query words: ";
        MList.print ~f:(fun s -> printf "\"%s\"" s) words;
        print_newline ();

        let keylist = Keydb.get_by_words ~max:200 words in
        List.iter ~f:(fun key ->
                        try
                          let keyid = Fingerprint.keyid_from_key key in
                          let keyidstr = Fingerprint.keyid_to_string
                                           ~short:true keyid in
                          printf "0x%s: %s\n"
                            keyidstr (List.hd (Key.get_ids key))
                        with
                            Not_found ->
                              printf "Failure to extract key\n";
                     )
          keylist;
      with
          e -> raise e
    done

  with
    | Exit -> Keydb.close_dbs (); print_string "Exiting.\n"
    | e -> Keydb.close_dbs ();
        print_string "Exiting by exception.\n";
        raise e

