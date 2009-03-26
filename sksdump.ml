(************************************************************************)
(* This file is part of SKS.  SKS is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA *)
(***********************************************************************)

(** takes content of SKS keyserver and creates key dump files from that *)

module F(M:sig end) = 
struct
  open StdLabels
  open MoreLabels
  open Printf
  open Common
  open Packet

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  module Keydb = Keydb.Unsafe

  let should_dump skey = match skey with
    | Keydb.KeyString _ | Keydb.Key _ -> true
    | Keydb.Offset _  | Keydb.LargeOffset _ ->
	if !Settings.dump_new then false else true

  let rec write_to_file size stream cout = 
    if size <= 0 then ()
    else
      match SStream.next stream with
	| None -> ()
	| Some (hash,string) ->
	    let remain =
	    try
	      let skey = Keydb.skey_of_string string in
	      if should_dump skey then
		let keystring = Keydb.keystring_of_skey skey in
		output_string cout keystring;
		size - 1
	      else
		size
	    with
		e -> 
		  eplerror 1 e "Failed attempt to extract key %s" 
		  (KeyHash.hexify hash);
		  size
	    in
	    write_to_file remain stream cout


  let write_to_fname size stream fname = 
    printf "Dumping keys to file %s\n" fname;
    flush stdout;
    let file = open_out fname in
    protect ~f:(fun () -> write_to_file size stream file)
      ~finally:(fun () -> close_out file)
      
  let dump_database dumpdir size name =
    let (stream,close) = Keydb.create_hash_skey_stream () in
    let run () = 
      let ctr = ref 0 in
      while SStream.peek stream <> None do
	let fname = 
	  Filename.concat dumpdir (sprintf "%s-%04d.pgp" name !ctr) 
	in
	write_to_fname size stream fname;
	incr ctr
      done
    in
    protect ~f:run ~finally:close
      


  exception Argument_error

  let run () = 
    try (
      match !Settings.anonlist with
	| size::dumpdir::tl ->
	    let name = match tl with
	      | [] -> "sks-dump"
	      | [name] -> name
	      | _ -> raise Argument_error
	    in
	    set_logfile "dump";
	    Keydb.open_dbs settings;
	    let size = int_of_string size in
	    dump_database dumpdir size name
	| _ -> 
	    raise Argument_error
    ) with Argument_error -> 
      eprintf "wrong number of arguments\n";
      eprintf "usage: sksdump numkeys dumpdir [dumpname]\n";
      flush stderr;
      exit (-1)
end
