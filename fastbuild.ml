(***********************************************************************)
(* fastbuild.ml - Executable: Builds up the key database from a multi- *)
(*                file database dump. This version works faster by     *)
(*                virtue of not actually copying the keys out of the   *)
(*                datbaase dump, and only storing the locations of     *)
(*                those keys.                                          *)
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
  module Unix = UnixLabels
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

  module Keydb = Keydb.Unsafe

  let n = match !Settings.n with 0 -> 1 | x -> x
  let maxkeys = n * 15000
  let dumpdir = Lazy.force Settings.dumpdir

  let lsdir dir =
    let dirhandle = Unix.opendir dir in
    let rec loop accum = match (try Some (Unix.readdir dirhandle)
                                with End_of_file -> None)
    with
        Some fname -> loop (fname::accum)
      | None -> accum
    in
    loop []

  let rec list_mapi list ~f =
    let rec loop list i ~f =
      match list with
          [] -> []
        | x::tl -> (f i x)::(loop tl (i + 1) ~f)
    in
    loop list 0 ~f

  let timestr sec =
    sprintf "%.2f min" (sec /. 60.)

  (******************************************************)

  type 'a badoption = Bad | Good of 'a | Done

  (** get single md using nextkey function *)
  let get_keymd fnum nextkey =
    match (try nextkey ()
           with e ->
             perror "error parsing key in file %d: %s.  Skipping rest of file"
             fnum (Printexc.to_string e);
             None
          )
    with
      | Some (pos,key) ->
          begin
            try
              let ckey = Fixkey.canonicalize key in

              if ckey = key then
                (* no need to canonicalize key *)

                let offset = { Keydb.fnum = fnum;
                               Keydb.pos = pos;
                             }
                in
                Good (Keydb.key_to_metadata_large_offset offset key)
              else
                (* must use canonicalized version of key *)
                Good (Keydb.key_to_metadata ckey)
            with
                Fixkey.Bad_key -> Bad
          end
      | None ->
          Done

  let rec get_keymds_rec ~max fnum nextkey accum =
    if max = 0
    then (accum,0)
    else
      match get_keymd fnum nextkey with
        | Done -> (accum,max)
        | Bad -> get_keymds_rec ~max fnum nextkey accum
        | Good md ->
            get_keymds_rec ~max:(max-1) fnum nextkey
            (md::accum)


  (** Fetches a collection of no more than max keys.  Returns (keys,bool), with
    the second argument being true of there is more to read from the given
    file. *)
  let rec get_keymds ~max fnum nextkey =
    get_keymds_rec ~max fnum nextkey []


  let inchan_to_nextkey inchan =
    let cin = new Channel.sys_in_channel inchan in
    Key.pos_next_of_channel cin

  let rec get_keymds_list ~max nflist partial =
    match nflist with
        [] -> (partial,[])
      | (fnum,nextkey)::tl ->
          if max = 0 then (partial,nflist)
          else
            let (mds,remaining) = get_keymds ~max fnum nextkey in
            flush stdout;
            if remaining > 0 then (
              (* file must be done with, so don't pass it on *)
              get_keymds_list ~max:remaining tl (List.rev_append mds partial)
            ) else (
              (* file is not (necessarily) done,
                 but we've got the key mds we need *)
              (List.rev_append mds partial,nflist)
            )

  let get_keymds_list ~max nflist = get_keymds_list ~max nflist []

  let dbtimer = MTimer.create ()
  let timer = MTimer.create ()

  (***************************************************************)

  let () = Sys.set_signal Sys.sigusr1 Sys.Signal_ignore
  let () = Sys.set_signal Sys.sigusr2 Sys.Signal_ignore

  (***************************************************************)
  let run () =
    set_logfile "fastbuild";
        perror "Running SKS %s%s" Common.version Common.version_suffix;

    if Sys.file_exists (Lazy.force Settings.dbdir) then (
      perror "KeyDB directory already exists.  Exiting.";
      eprintf "KeyDB directory already exists.  Exiting.\n";
      exit (-1)
    );
    Unix.mkdir (Lazy.force Settings.dbdir) 0o700;
    Utils.initdbconf !Settings.basedir (Lazy.force Settings.dbdir);

    Keydb.open_dbs settings;
    Keydb.set_meta ~key:"filters" ~data:"yminsky.dedup";

    let filearray = Keydb.get_dump_filearray () in
    let nfarray = Array.mapi ~f:(fun i x -> (i,inchan_to_nextkey x))
                    filearray in
    let nflist = Array.to_list nfarray in

    perror "Loading %d keys at a time" maxkeys;

    protect
      ~f:(fun () ->
            let rec loop nflist = match nflist with
                [] -> ()
              | nflist ->
                  MTimer.start timer;

                  perror "Loading metadata..."; flush stdout;
                  let (mds,nflist) = get_keymds_list ~max:maxkeys nflist in
                  perror "   %d keys loaded, %d files left"
                    (List.length mds) (List.length nflist);
                  MTimer.start dbtimer;
                  Keydb.add_mds mds;
                  MTimer.stop dbtimer;

                  MTimer.stop timer;
                  perror "   DB time:  %s.  Total time: %s."
                    (timestr (MTimer.read dbtimer))
                    (timestr (MTimer.read timer));
                  flush stdout;
                  loop nflist

            in
            loop nflist
         )
      ~finally:(fun () -> Keydb.close_dbs ())

end
