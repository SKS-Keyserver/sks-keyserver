(***********************************************************************)
(* reconComm.ml                                                        *)
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
open Packet

module Unix = UnixLabels
module Map = PMap.Map

open DbMessages

(***************************************************************)
(**  Message Sending Primitives  *)
(***************************************************************)

(** send DbMessages message and wait for response *)
let send_dbmsg msg =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr db_command_addr)
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
            ~domain:(Unix.domain_of_sockaddr db_command_addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0 in
  protect ~f:(fun () ->
                Unix.connect s ~addr:db_command_addr;
                let cout = Channel.sys_out_from_fd s in
                marshal cout msg )
    ~finally:(fun () -> Unix.close s)

let is_content_type line =
  try
    let colonpos = String.index line ':' in
    let prefix = String.sub ~pos:0 ~len:colonpos line in
    String.lowercase prefix = "content-type"
  with
      Not_found -> false

let http_status_ok_regexp = Str.regexp "^HTTP/[0-9]+\\.[0-9]+ 2"

let get_keystrings_via_http addr hashes =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0  in
  protect ~f:(fun () ->
                Unix.bind s ~addr:(match_client_recon_addr addr);
                Unix.connect s ~addr;
                let cin = Channel.sys_in_from_fd s
                and cout = Channel.sys_out_from_fd s in

                let sout = Channel.new_buffer_outc 0 in
                CMarshal.marshal_list ~f:CMarshal.marshal_string sout hashes;
                let msg = sout#contents in
                cout#write_string "POST /pks/hashquery HTTP/1.0\r\n";
                cout#write_string (sprintf "content-length: %d\r\n\r\n"
                                     (String.length msg));
                cout#write_string msg;
                cout#flush;
                (* read "HTTP" line and make sure the status is 2xx *)
                let status = input_line cin#inchan in
                if not (Str.string_match http_status_ok_regexp status 0) then
                  failwith status;
                let _headers = Wserver.parse_headers Map.empty cin#inchan in
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


