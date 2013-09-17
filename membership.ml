(***********************************************************************)
(* membership.ml - Simple module for loading membership information.   *)
(*                 Currently only loads membership from membership     *)
(*                 file.                                               *)
(*                 @author Yaron M. Minsky                             *)
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
module Unix=UnixLabels
open Printf
open Scanf
open Common

exception Bug of string
exception Lookup_failure of string
exception Malformed_entry of string
exception Empty_line

let membership = ref ([| |],-1.)

let whitespace = Str.regexp "[ \t]+"

let lookup_hostname string service =
  Unix.getaddrinfo string service [Unix.AI_SOCKTYPE Unix.SOCK_STREAM]

let local_recon_addr () =
  lookup_hostname !Settings.hostname (string_of_int recon_port)

let local_recon_addr = Utils.unit_memoize local_recon_addr

let convert_address l =
  try
    if String.length l = 0 then raise Empty_line else
    sscanf l "%s %s"
      (fun addr service ->
         if addr = "" || service = "" then failwith "Blank line";
         addr, service)
  with
    Scanf.Scan_failure _ | End_of_file | Failure _ -> raise (Malformed_entry l)

let load_membership_file file =
  let rec loop list =
    try
      let line = decomment (input_line file) in
      let addr = convert_address line in
      addr :: loop list
    with
      | Empty_line -> loop list
      | End_of_file -> list
      | Malformed_entry line ->
          perror "Malformed entry %s" line;
          loop list
  in
  loop []

let get_mtime fname =
  try
    if Sys.file_exists fname
    then Some (Unix.stat fname).Unix.st_mtime
    else None
  with
      Unix.Unix_error _ -> None

let load_membership fname =
  let file = open_in fname in
  protect ~f:(fun () ->
    load_membership_file file)
    ~finally:(fun () -> close_in file)

let ai_to_string = function
  | { Unix.ai_addr = Unix.ADDR_UNIX s } -> sprintf "<ADDR_UNIX %s>" s
  | { Unix.ai_addr = Unix.ADDR_INET (addr,p) } -> sprintf "<ADDR_INET [%s]:%d>"
        (Unix.string_of_inet_addr addr) p

let ai_list_to_string ai_list =
  "[" ^ (String.concat ~sep:", " (List.map ~f:ai_to_string ai_list)) ^ "]"

let membership_string () =
  let (mshp,_) = !membership in
  let to_string (addr, (host, service)) =
    sprintf "(%s %s)%s" host service (ai_list_to_string addr)
  in
  let strings = List.map ~f:to_string (Array.to_list mshp) in
  "Membership: " ^ String.concat ~sep:", " strings

(* Refresh member n's address *)
let refresh_member members n =
  match members.(n) with
    (addr, (host, service as line)) ->
      let fresh_addr = lookup_hostname host service in
      if addr <> fresh_addr then begin
        members.(n) <- (fresh_addr, line);
        plerror 3 "address for %s:%s changed from %s to %s"
          host service (ai_list_to_string addr) (ai_list_to_string fresh_addr)
      end

let reload_if_changed () =
  let fname = Lazy.force Settings.membership_file in
  let (mshp,old_mtime) = !membership in
  match get_mtime fname with
    | None ->
        plerror 2 "%s" ("Unable to get mtime for membership file. " ^
                        "Can't decide whether to reload")
    | Some mtime ->
        if old_mtime <> mtime then
          ( let memberlines = load_membership fname in
          let old = Array.to_list mshp in
          let f line =
            try
              List.find ~f:(fun (_, old_line) -> line = old_line) old
            with
              Not_found -> ([], line)
          in
          let merged = Array.of_list (List.map ~f memberlines) in
          membership := (merged, mtime);
          plerror 5 "%s" (membership_string ());
          (* Try to lookup unknown names *)
          Array.iteri
              ~f:(fun i mb -> if fst mb = [] then refresh_member merged i)
              merged
          )

let get_names () =
  let file = Lazy.force Settings.membership_file in
  let mshp =
    if not (Sys.file_exists file) then [||]
    else (
      reload_if_changed ();
      let (m,_) = !membership in
      m
    )
  in
  Array.map ~f:(function (_, (host, service)) -> host ^ " " ^ service) mshp


let reset_membership_time () =
  let (m,mtime) = !membership in
  membership := (m,0.)

let same_inet_addr addr1 addr2 =
  match (addr1,addr2) with
      (Unix.ADDR_INET (ip1,_), Unix.ADDR_INET (ip2,_)) -> ip1 = ip2
    | _ -> false

let rec choose () =
  if Sys.file_exists (Lazy.force Settings.membership_file) then begin
    reload_if_changed ();
    let (mshp, _) = !membership in
    let choice = Random.int (Array.length mshp) in
    refresh_member mshp choice;
    match fst mshp.(choice) with
      [] -> choose ()
    | addrlist ->
        let saddr = (List.hd addrlist).Unix.ai_addr in
        let same_addr thisaddr = same_inet_addr saddr thisaddr.Unix.ai_addr in
        if List.exists ~f:same_addr (local_recon_addr ()) then
          choose () else
          addrlist
  end else
    raise Not_found

let test addr =
  reload_if_changed ();
  let (m,_) = !membership in
  let same_as_addr this_addr = same_inet_addr addr this_addr.Unix.ai_addr in
  List.exists (Array.to_list m)
    ~f:(fun x -> List.exists ~f:same_as_addr (fst x))

(************************************************************)
(** Code for keeping track of hosts to send mail updates to *)
(************************************************************)

let mailsync_partners = ref ([ ],-1.)

let rec load_mailsync_partners_file file =
  try
    let email = Wserver.strip (decomment (input_line file)) in
    if String.contains email '@'
    then email::(load_mailsync_partners_file file)
    else load_mailsync_partners_file file
  with
      End_of_file -> []

let load_mailsync_partners fname =
  let file = open_in fname in
  let run () =
    match get_mtime fname with
      | Some mtime ->
          mailsync_partners := (load_mailsync_partners_file file,mtime)
      | None ->
          plerror 2 "Failed to find mtime -- can't load mailsync file"
  in
  protect ~f:run ~finally:(fun () -> close_in file)

let reload_mailsync_if_changed () =
  let fname = Lazy.force Settings.mailsync_file in
  let (msync,old_mtime) = !mailsync_partners in
  match get_mtime fname with
      None -> if !Settings.send_mailsyncs then plerror 2 "%s"
        ("Failed to find mtime, can't decide whether to" ^
         " load mailsync file")
    | Some mtime -> if old_mtime <> mtime then load_mailsync_partners fname

let get_mailsync_partners () =
  let partners =
    if Sys.file_exists (Lazy.force Settings.membership_file) then (
      reload_mailsync_if_changed ();
      let (m,mtime) = !mailsync_partners in
      m
    )
    else []
  in
  if partners = [] then failwith "No partners specified"
  else partners
