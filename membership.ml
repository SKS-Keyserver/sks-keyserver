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

(** Simple module for loading membership information.  Currently only loads
  membership from membership file.
  @author Yaron M. Minsky
*)
open StdLabels
open MoreLabels
module Unix=UnixLabels
open Printf
open Scanf
open Common

exception Bug of string
exception Lookup_failure of string
exception Malformed_entry of string

let membership = ref ([| |],-1.)

let whitespace = Str.regexp "[ \t]+"

let lookup_hostname string = 
  try (Unix.gethostbyname string).Unix.h_addr_list.(0)
  with 
    | Invalid_argument _ | Not_found -> raise (Lookup_failure string)

let local_recon_addr () = 
  Unix.ADDR_INET (lookup_hostname !Settings.hostname, recon_port)

let local_recon_addr = Utils.unit_memoize local_recon_addr

let remove_self addresses = 
  List.filter ~f:(fun (addr,str) -> addr <> local_recon_addr ()) addresses

let convert_address l =
  try 
    sscanf l "%s %d" 
    (fun addr port -> Unix.ADDR_INET (lookup_hostname addr,port))
  with 
    Scanf.Scan_failure _ | End_of_file | Failure _ -> raise (Malformed_entry l)

let load_membership_file file =
  let rec loop list =
    try
      let line = Wserver.strip (decomment (input_line file)) in
      if String.length line > 0 then
        let addr = convert_address line in
        (addr,line) :: loop list
      else
        loop list
    with
      | End_of_file -> list
      | Lookup_failure addr -> 
	  perror "Lookup failure on address %s" addr;
	  loop list
      | Malformed_entry line -> 
	  perror "Malformed entry %s" line;
	  loop list
  in
  let raw_membership = loop [] in
  Array.of_list (remove_self raw_membership)

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
		let mshp = load_membership_file file in
		match get_mtime fname with
		  | None -> 
		      plerror 2 "%s" 
			("Unable to get mtime for membership. " ^
			 "Failed to reload.")
		  | Some mtime -> membership := (mshp,mtime)
	     )
    ~finally:(fun () -> close_in file)

let sockaddr_to_string sockaddr = match sockaddr with
    Unix.ADDR_UNIX s -> sprintf "<ADDR_UNIX %s>" s
  | Unix.ADDR_INET (addr,p) -> sprintf "<ADDR_INET %s:%d>" 
      (Unix.string_of_inet_addr addr) p

let membership_string () = 
  let (mshp,_) = !membership in
  let to_string (addr,str) =
    sprintf "%s(%s)" (sockaddr_to_string addr) str
  in
  let strings = List.map ~f:to_string (Array.to_list mshp) in
  "Membership: " ^ String.concat ~sep:", " strings
    

let reload_if_changed () = 
  let fname = Lazy.force Settings.membership_file in
  let (mshp,old_mtime) = !membership in
  match get_mtime fname with
    | None -> 
	plerror 2 "%s" ("Unable to get mtime for membership file. " ^
			"Can't decide whether to reload")
    | Some mtime ->
	if old_mtime <> mtime then 
	  ( load_membership fname;
	    plerror 5 "%s" (membership_string ())
	  )

let get_names () = 
  let mshp = 
    if Sys.file_exists (Lazy.force Settings.membership_file) then (
      reload_if_changed ();
      let (m,mtime) = !membership in 
      m
    )
    else [| |]
  in
  Array.map ~f:snd mshp


let reset_membership_time () =
  let (m,mtime) = !membership in
  membership := (m,0.)

let get () = 
  let mshp = 
    if Sys.file_exists (Lazy.force Settings.membership_file) then (
      reload_if_changed ();
      let (m,mtime) = !membership in 
      m
    )
    else [| |]
  in
  Array.map ~f:fst mshp

let same_inet_addr addr1 addr2 = 
  match (addr1,addr2) with
      (Unix.ADDR_INET (ip1,_), Unix.ADDR_INET (ip2,_)) -> ip1 = ip2
    | _ -> false

let test addr = 
  reload_if_changed ();
  let (m,mtime) = !membership in
  
  let found = ref false in
  let i = ref 0 in
  while !i < Array.length m && not !found do 
    if same_inet_addr addr (fst m.(!i)) then
      found := true;
    incr i
  done;
  !found



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
  let (mshp,old_mtime) = !membership in
  match get_mtime fname with
      None -> plerror 2 "%s" 
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
