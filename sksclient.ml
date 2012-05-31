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

open StdLabels
open MoreLabels
open Printf
open Common
open DbMessages

let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
}

module Keydb = Keydb.Safe

let get_keys_by_keyid keyid =
    let keyid_length = String.length keyid in
    let short_keyid = String.sub ~pos:(keyid_length - 4) ~len:4 keyid in
    let keys = Keydb.get_by_short_subkeyid short_keyid in
    match keyid_length with
      | 4 -> (* 32-bit keyid.  No further filtering required. *)
	  keys

      | 8 -> (* 64-bit keyid *)
	   List.filter keys
	   ~f:(fun key -> keyid = (Fingerprint.from_key key).Fingerprint.keyid ||
	   (** Return keys i& subkeys with matching long keyID *)
	     let (mainkeyid,subkeyids) = Fingerprint.keyids_from_key ~short:false key in
	     List.exists (fun x -> x = keyid) subkeyids)

      | _ -> failwith "Unknown keyid type"

let dump_one_key keyid =
	begin 
	let deprefixed = 
		if String.length keyid > 2 then
			if String.sub keyid 0 2 = "0x" then
			String.sub keyid 2 (String.length keyid - 2)
			else keyid
	    else exit 3
	in
	let keys = get_keys_by_keyid (KeyHash.dehexify deprefixed) in
	let count = List.length keys in
	if count < 1 then
	 exit 2;
	
	let aakeys =
	    match keys with
	      | [] -> ""
	      | _ -> let keystr = Key.to_string_multiple keys in
		      Armor.encode_pubkey_string keystr
	  in
	printf "%s\n" aakeys;
    end 
	
let keysource action =
    if !Settings.use_stdin then
	try
	    while true do
		let line = input_line stdin in
		action line;
	    done;
	with
	End_of_file -> printf "\n";
    else
	(
	    let len = Array.length Sys.argv in
	    let params = Array.sub Sys.argv 1 (len-1) in
	    Array.iter action params;
	)

let () =
    if (Array.length Sys.argv) < 2 then
	failwith "Keys in argv unless -stdin set";
	
    set_logfile "sksclient";
    Keydb.open_dbs settings;
    keysource dump_one_key;
    Keydb.close_dbs ();
