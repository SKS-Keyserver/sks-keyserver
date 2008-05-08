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
open Packet

module Unix = UnixLabels
open DbMessages

(***************************************************************)
(**  Message Sending Primitives  *)
(***************************************************************)

(** send DbMessages message and wait for response *)
let send_dbmsg msg = 
  let ctr = ref 0 in
  let s = Unix.socket 
	    ~domain:Unix.PF_UNIX 
	    ~kind:Unix.SOCK_STREAM 
	    ~protocol:0 in
  protect ~f:(fun () ->
		Unix.connect s ~addr:db_command_addr;
		let cin = Channel.sys_in_from_fd s in
		let cout = Channel.sys_out_from_fd s in
		marshal cout msg;
		let reply = (unmarshal cin).msg in
		reply
	     )
    ~finally:(fun () -> Unix.close s)
    

(** send DbMessages message, don't wait for response *)
let send_dbmsg_noreply msg = 
  let s = Unix.socket 
	    ~domain:Unix.PF_UNIX
	    ~kind:Unix.SOCK_STREAM 
	    ~protocol:0 in
  protect ~f:(fun () ->
		Unix.connect s ~addr:db_command_addr;
		let cin = Channel.sys_in_from_fd s
		and cout = Channel.sys_out_from_fd s in
		marshal cout msg )
    ~finally:(fun () -> Unix.close s)

let is_content_type line = 
  try
    let colonpos = String.index line ':' in
    let prefix = String.sub ~pos:0 ~len:colonpos line in
    String.lowercase prefix = "content-type"
  with
      Not_found -> false

let get_keystrings_via_http addr hashes = 
  let s = Unix.socket 
	    ~domain:Unix.PF_INET 
	    ~kind:Unix.SOCK_STREAM 
	    ~protocol:0  in
  protect ~f:(fun () -> 
		Unix.bind s ~addr:(get_client_recon_addr ());
		Unix.connect s ~addr;
		let cin = Channel.sys_in_from_fd s 
		and cout = Channel.sys_out_from_fd s in

		let sout = Channel.new_buffer_outc 0 in
		CMarshal.marshal_list ~f:CMarshal.marshal_string sout hashes;
		let msg = sout#contents in
		cout#write_string "POST /pks/hashquery\r\n";
		cout#write_string (sprintf "content-length: %d\r\n\r\n" 
				     (String.length msg));
		cout#write_string msg;
		cout#flush;
		ignore (input_line cin#inchan); (* read "HTTP" line *)
		let headers = Wserver.parse_headers Map.empty cin#inchan in
		let keystrings = 
		  CMarshal.unmarshal_list ~f:CMarshal.unmarshal_string cin
		in
		keystrings
	     )
    ~finally:(fun () -> Unix.close s)



let fetch_filters () = 
  let reply = send_dbmsg (Config ("filters",`none)) in
  match reply with
    | Filters filters -> filters
    | _ -> failwith "ReconComm.fetch_filters: unexpected reply"


