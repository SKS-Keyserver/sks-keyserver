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

(** Code for generating machine-readable index *)

open StdLabels
open MoreLabels
open Printf

open Common
open Packet

let mr_version = 1

(** Does escaping of uid strings *)
let escape_uid_string string =
  let buf = Buffer.create (String.length string) in
  for i = 0 to String.length string - 1 do
    if string.[i] = '%' then (
      Buffer.add_char buf '%';
      Buffer.add_char buf string.[i]
    ) 
    else if int_of_char string.[i] >= 128 || string.[i] = ':' then
      let v = int_of_char string.[i] in
      Buffer.add_string buf (sprintf "%%%X" v)
    else
      Buffer.add_char buf string.[i]
  done;
  Buffer.contents buf
    
let get_signature_keyid sign = 
  match sign with
    | V3sig s -> Some s.v3s_keyid
    | V4sig s -> 
	let issuer_subpackets = 
	  List.filter ~f:(fun ssp -> ssp.ssp_type = 16) 
	    (s.v4s_hashed_subpackets @ s.v4s_unhashed_subpackets)
	in
	match issuer_subpackets with
	  | [ssp] -> 
	      if String.length ssp.ssp_body = 8 
	      then Some ssp.ssp_body else None
	  | _ -> None


let get_sigtype sign = match sign with
    V3sig sign -> sign.v3s_sigtype | V4sig sign -> sign.v4s_sigtype 

let get_self_sigs keyid sigs = 
  let sigs = List.map ~f:ParsePGP.parse_signature sigs in
  List.filter 
    ~f:(fun sign -> 
	  (match int_to_sigtype (get_sigtype sign) with
	     | Generic_certification_of_a_User_ID_and_Public_Key_packet 
	     | Persona_certification_of_a_User_ID_and_Public_Key_packet 
	     | Casual_certification_of_a_User_ID_and_Public_Key_packet 
	     | Positive_certification_of_a_User_ID_and_Public_Key_packet 
	       -> true 
	     | _ -> false) && 
	  (match get_signature_keyid sign with
	     | Some sig_keyid -> sig_keyid = keyid 
	     | None -> false)
       )
    sigs

let time_to_string time = match time with
  | None -> ""
  | Some x -> sprintf "%Ld" x

let uid_to_line keyid uid_packet sigs =
  let uid_string = escape_uid_string uid_packet.packet_body in
  let sigs = get_self_sigs keyid sigs in
  let times = List.map ~f:ParsePGP.get_times sigs in
  let (ctime,exptime) = 
    List.fold_left ~init:(None,None) ~f:max times
  in
  sprintf "uid:%s:%s:%s:"
    uid_string (time_to_string ctime) (time_to_string exptime)
	
(** number of seconds in a day *)
let daysecs = Int64.of_int (60 * 60 * 24)

let key_to_lines key = 
  let full_keyid = Fingerprint.keyid_from_key ~short:false key in
  let keyid = Fingerprint.keyid_to_string ~short:true full_keyid in
  let pkey = KeyMerge.key_to_pkey key in
  let key_packet = pkey.KeyMerge.key in
  let pki = ParsePGP.parse_pubkey_info key_packet in
  let uids = pkey.KeyMerge.uids in 
  let exp_string = match pki.pk_expiration with
    | None -> ""
    | Some 0 -> "-"
    | Some days -> sprintf "%Ld"
	(Int64.add pki.pk_ctime (Int64.mul daysecs (Int64.of_int days)))
  in
  let key_line = sprintf "pub:%s:%d:%d:%Ld:%s:%s"
		   keyid 
		   pki.pk_alg 
		   pki.pk_keylen
		   pki.pk_ctime
		   exp_string
		   (if (Index.is_revoked key) then "r" else "")
  in
  let uid_lines = 
    List.map ~f:(fun (uid,sigs) -> 
      match uid.packet_type with
          User_ID_Packet -> uid_to_line full_keyid uid sigs
	| User_Attribute_Packet -> "uat::::"
	| _ -> "???::::"
      ) uids
  in
  key_line::uid_lines


let keys_to_lines keys = 
  let first = sprintf "info:%d:%d" mr_version (List.length keys) in
  let keylines = List.concat (List.map ~f:key_to_lines keys) in
  first::keylines

let keys_to_index keys = 
  (String.concat ~sep:"\n" (keys_to_lines keys)) ^ "\n"
