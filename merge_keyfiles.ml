(***********************************************************************)
(* merge_keyfiles.ml - Executable: Adds keys from key files to         *)
(*                     existing database.                              *)
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

module F(M:sig end) =
struct
  open StdLabels
  open MoreLabels
  open Printf
  open Arg
  open Common
  module Set = PSet.Set
  open Packet

  let settings = {
    Keydb.withtxn = false;
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

  module Keydb = Keydb.Safe

  let n = match !Settings.n with 0 -> 1 | x -> x
  let maxkeys = n * 15000
  let fnames = List.filter ~f:(fun x -> x <> "") (List.rev !Settings.anonlist)

  let timestr sec =
    sprintf "%.2f min" (sec /. 60.)

  (* ******************************************************************** *)
  (** data type and functions for dealing with collection of files as
    one big stream *)

  type keydump_stream =
      { getkey: unit -> packet list;
        current: in_channel;
        fnames: string list;
        ctr: int;
      }

  let create_keydump_stream ctr fnames =
    match fnames with
      | [] -> raise End_of_file
      | hd::tl ->
          let file = open_in hd in
          let cin = new Channel.sys_in_channel file in
          let getkey = Key.get_of_channel cin in
          { getkey = getkey;
            current = file;
            fnames = tl;
            ctr = ctr;
          }

  let rec get_key stream =
    try (!stream).getkey ()
    with Not_found | End_of_file ->
      close_in (!stream).current;
      stream := create_keydump_stream ((!stream).ctr + 1) (!stream).fnames;
      get_key stream

  let create_keydump_stream fnames = ref (create_keydump_stream 0 fnames)

  let lpush el list = list := el::!list

  let get_n_keys stream n =
    let data = ref [] in
    (try
       for i = 1 to n do
         lpush (get_key stream) data
       done
     with
         End_of_file ->
           stream := { !stream with getkey = (fun () -> raise End_of_file) }
    );
    !data

  (* *************************************************** *)

  let dbtimer = MTimer.create ()
  let timer = MTimer.create ()
  let run () =
    set_logfile "merge";
        perror "Running SKS %s%s" Common.version Common.version_suffix;
    if not (Sys.file_exists (Lazy.force Settings.dbdir)) then (
      printf "No existing KeyDB database.  Exiting.\n";
      exit (-1)
    );

    Keydb.open_dbs settings;
    if fnames = [] then failwith "No files provided";
    let finished = ref false in
    let stream = create_keydump_stream fnames in
    try
      protect
        ~f:(fun () ->
              while not !finished do

                MTimer.start timer;

                printf "Loading keys...\n"; flush stdout;
                let keys = get_n_keys stream maxkeys in
                if keys = [] then raise Exit;
                printf "   %d keys loaded, %d files left\n"
                  (List.length keys) (List.length !stream.fnames);
                flush stdout;

                MTimer.start dbtimer;
                Keydb.add_keys_merge keys;
                MTimer.stop dbtimer;

                MTimer.stop timer;

                printf "   DB time:  %s.  Total time: %s.\n"
                  (timestr (MTimer.read dbtimer))
                  (timestr (MTimer.read timer));
                flush stdout;
              done
           )
        ~finally:(fun () ->
                    perror "closing database...";
                    Keydb.close_dbs ();
                    perror "...database closed";
                 )
    with
        Exit -> ()
end
