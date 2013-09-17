(***********************************************************************)
(* incdump.ml - creates keydump consisting of recently added keys      *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012, 2013  Yaron Minsky and Contributors       *)
(* Copyright (C) 2004 Peter Palfrader                                  *)
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
module Set = PSet.Set

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

module Keydb = Keydb.Unsafe

let dump_database timestamp fname =
  let maxsize = 250_000 in
  let log = Keydb.reverse_logquery ~maxsize timestamp in
  if List.length log = 0 then
    printf "No changes since timestamp\n"
  else
    let file = open_out fname in
    let run () =
      let newkeys = List.fold_left log ~init:Set.empty
                      ~f:(fun set (_,change) -> match change with
                              Add hash -> Set.add hash set
                            | Delete hash -> Set.remove hash set)
      in
      printf "%d new keys in log.\n%!" (Set.cardinal newkeys);
      Set.iter newkeys
        ~f:(fun hash ->
              try
                let keystring = Keydb.get_keystring_by_hash hash in
                output_string file keystring;
              with
                  e ->
                    eprintf "Error fetching keystring from hash %s: %s\n%!"
                    (Utils.hexstring hash)
                    (Printexc.to_string e)
           )
    in
    protect ~f:run ~finally:(fun () -> close_out file)

let run () =
  List.iter !Settings.anonlist
    ~f:(fun x -> printf "\"%s\" " x);
  printf "\n%!";
  match !Settings.anonlist with
    | timestamp::tl ->
        let name = match tl with
          | [] -> "incdump.pgp"
          | [name] -> name
          | _ -> raise (Argument_error "too many arguments")
        in
        printf "saving to file %s\n%!" name;
        set_logfile "incdump";
        perror "Running SKS %s%s" Common.version Common.version_suffix;
        Keydb.open_dbs settings;
        protect ~f:(fun () ->
                      let timestamp = float_of_string timestamp in
                      dump_database timestamp name )
          ~finally:(fun () -> Keydb.close_dbs ())

    | _ ->
        raise (Argument_error "no timestamp provided")

