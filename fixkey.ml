(***********************************************************************)
(* fixkey.ml                                                           *)
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
open Common
open Packet

module Map = PMap.Map

exception Bad_key
exception Standalone_revocation_certificate


(** list of filters currently applied on incoming keys.  Filter types are
  included in comma-separated list, and should not include commas or
  whitespace

  meaning of filter types:

  - yminsky.merge:
      Merges all keys in database that can be merged.
  - yminsky.dedup:
      Parses all keys and removes duplicates.  Unparseable keys
      are removed from the database.
*)
let filters = [ "yminsky.dedup"; "yminsky.merge" ]

(**********************************************************************)
(***  Key Merging  ****************************************************)
(**********************************************************************)

let get_keypacket pkey = pkey.KeyMerge.key

let ( |= ) map key = Map.find key map
let ( |< ) map (key,data) = Map.add ~key ~data map

let rec join_by_keypacket map keylist = match keylist with
  | [] -> map
  | key::tl ->
      let keypacket = get_keypacket key in
      let map =
        try
          let keylist_ref = map |= keypacket in
          keylist_ref := key::!keylist_ref;
          map
        with
            Not_found ->
              map |< (keypacket,ref [key])
      in
      join_by_keypacket map tl

(** Given a list of parsed keys, returns a list of parsed key lists,
  grouped by keypacket *)
let join_by_keypacket keys =
  Map.fold ~f:(fun ~key ~data list -> !data::list) ~init:[]
    (join_by_keypacket Map.empty keys)


(** merges a list of pkeys, throwing a failure if the merge cannot procede *)
let merge_pkeys pkeys = match pkeys with
  | [] -> failwith "Attempt to merge empty list of keys"
  | hd::tl ->
      List.fold_left ~init:hd tl
      ~f:(fun key1 key2 ->
            match KeyMerge.merge_pkeys key1 key2 with
                None -> failwith "PKey merge failed"
              | Some key -> key
         )

(** Accepts collection of keys, which should comprise all keys in the
  database with the same keyid.  Returns list of pairs, first part of pair
  being a list of keys to delete, last part being a list of keys to add
*)
let compute_merge_replacements keys =
  let pkeys = List.map ~f:KeyMerge.key_to_pkey keys in
  (* put parsed keys into list of lists, grouped by key packet *)
  let kp_list = join_by_keypacket pkeys in
  let replacements =
    List.fold_left ~init:[] kp_list
      ~f:(fun list pkeys ->
            if List.length pkeys > 1 then
              (Some (List.map ~f:KeyMerge.flatten pkeys,
                     KeyMerge.flatten (merge_pkeys pkeys)))::list
            else
              None::list
         )
  in
  strip_opt replacements


(**********************************************************************)
(***  Key Canonicalization  *******************************************)
(**********************************************************************)

(** Returns canonicalized version of key.  Raises Bad_key if key should simply
  be discarded
*)
let is_revocation_signature pack =
   match pack.packet_type with
    | Signature_Packet ->
      let parsed_signature = ParsePGP.parse_signature pack in
      let sigtype = match parsed_signature with
       | V3sig s -> s.v3s_sigtype
       | V4sig s -> s.v4s_sigtype
     in
     let result =  match (int_to_sigtype sigtype) with
           | Key_revocation_signature | Subkey_revocation_signature
             | Certification_revocation_signature -> true
           | _ -> false
     in
     result
    | _ -> false

let canonicalize key =
  if is_revocation_signature (List.hd key)
    then raise Standalone_revocation_certificate;
  try KeyMerge.dedup_key key
  with KeyMerge.Unparseable_packet_sequence -> raise Bad_key


open KeyMerge

let good_key pack =
  try ignore (ParsePGP.parse_pubkey_info pack); true
  with e -> false

let good_signature pack =
  try ignore (ParsePGP.parse_signature pack); true
  with e -> false

let drop_bad_sigs packlist =
  List.filter ~f:good_signature packlist

let sig_filter_sigpair (pack,sigs) =
  let sigs = List.filter ~f:good_signature sigs in
  if sigs = [] then None
  else Some (pack,sigs)

let presentation_filter key =
  let pkey = key_to_pkey key in
  if not (good_key pkey.key)
  then None
  else
    let selfsigs = drop_bad_sigs pkey.selfsigs in
    let subkeys = Utils.filter_map ~f:sig_filter_sigpair pkey.subkeys in
    let uids = Utils.filter_map ~f:sig_filter_sigpair pkey.uids in
    let subkeys = List.filter ~f:(fun (key,_) -> good_key key) subkeys in
    Some (flatten { pkey with
                      selfsigs = selfsigs;
                      uids = uids;
                      subkeys = subkeys;
                  })
