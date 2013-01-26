(***********************************************************************)
(* fingerprint.ml - Computes PGP fingerprints and keyids               *)
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

open Printf
open StdLabels
open MoreLabels
open Common

open Packet
module Set = PSet.Set

(* Compute PGP Key Fingerprint and PGP KeyIDs *)

(* v3 and v4 fingerprints and keyids are quite different.

   v3 fingerprint: MD5 sum of concatenation of bodies of MPI's
                   for modulus and exponent of RSA key

   v3 keyid: low 64 bits of public modulus of RSA key

   v4 fingerprint: 160-bit SHA-1 hash of:
        Packet Tag (1 octet)
        packet length (2 octets)
        entire public key packet (starting with version field)

   v4 KeyID: first 64 bits of fingerprint
*)


type result = { fp : string;
                keyid : string;
              }

let from_packet packet =
  let cin = new Channel.string_in_channel packet.packet_body 0 in
  let version = cin#read_byte in
  match version with
      2 | 3 ->
        let hash = Cryptokit.Hash.md5 () in
        (* print_string "v3 pubkey\n"; *)
        cin#skip 7;
        (* skip creation time (4 octets), days of validity (2 octets)
           and algorithm type (1 octet) *)
        let n = ParsePGP.read_mpi cin in (* modulus *)
        let e = ParsePGP.read_mpi cin in (* exponent *)
        hash#add_substring n.mpi_data 0 ((n.mpi_bits + 7)/8);
        hash#add_substring e.mpi_data 0 ((e.mpi_bits + 7)/8);
        let fingerprint = hash#result
        and keyid =
          let len = String.length n.mpi_data in
          String.sub n.mpi_data ~pos:(len - 8) ~len:8
        in
        hash#wipe;
        { fp = fingerprint;
          keyid = keyid;
        }

    | 4 ->
        let hash = Cryptokit.Hash.sha1 () in
        hash#add_byte 0x99;
        (* This seems wrong.  The spec suggests that packet.packet_tag
           is what should be used here.  But this is what's done in the GPG
           codebase, so I'm copying it. *)
        hash#add_byte ((packet.packet_length lsr 8) land 0xFF);
        hash#add_byte (packet.packet_length land 0xFF);
        hash#add_string packet.packet_body;
        let fingerprint = hash#result in
        let keyid =
          let len = String.length fingerprint in
          String.sub fingerprint ~pos:(len - 8) ~len:8
        in
        hash#wipe;
        { fp = fingerprint;
          keyid = keyid;
        }

    | _ ->
        failwith "Fingerprint.from_packet: Unexpected version number"

let rec from_key key = match key with
    packet::key_tail ->
      if  packet.packet_type = Public_Key_Packet
      then from_packet packet
      else from_key key_tail
  | [] ->
      raise Not_found

let fp_to_string fp =
  let bs = if (String.length fp) = 20 then 4 else 2 in
  (* standard practice is to bunch long fingerprints by 4 and short ones by
     2.  An extra space is added in the middle *)
  let hex = Utils.hexstring fp in
  let buf = Buffer.create 0 in
  let extraspace_pos = if (String.length fp) = 20 then 4 else 7 in
  for i = 0 to String.length hex / bs - 1 do
    Buffer.add_substring buf hex (i * bs) bs;
    Buffer.add_string buf " ";
    if i = extraspace_pos then Buffer.add_string buf " "
  done;
  Buffer.contents buf

let keyid_to_string ?(short=true) keyid =
  let hex = Utils.hexstring keyid in
  if short
  then String.sub ~pos:(String.length hex - 8) ~len:8 hex
  else hex

let max32 = Int64.shift_left Int64.one 32
let is_32bit int64 =
  int64 < max32

let keyid32_of_string s =
  let s =
    if not (s.[0] = '0' && s.[1] = 'x')
    then "0x" ^ s else s
  in
  let x = Int64.of_string s in
  let x = Int64.to_int32 x in
  let cout = Channel.new_buffer_outc 4 in
  cout#write_int32 x;
  cout#contents

let keyid_of_string s =
  let x = Int64.of_string s in
  if is_32bit x then (
    let x = Int64.to_int32 x in
    let cout = Channel.new_buffer_outc 4 in
    cout#write_int32 x;
    cout#contents
  ) else (
    let cout = Channel.new_buffer_outc 8 in
    cout#write_int64 x;
    cout#contents
  )

let shorten ~short keyid =
  if short then String.sub ~pos:4 ~len:4 keyid else keyid

let fp_from_key key = (from_key key).fp
let keyid_from_key ?(short=true) key =
  let keyid = (from_key key).keyid in
  shorten ~short keyid

(** Returns a pair of the [result]s describing the fingerprint of the public key
    paired with the list of results describing the fingerprints of the subkeys.
    Raises `Not_found` if the information in question can't be found *)
let key_and_subkey_results key =
  match key with
  | [] -> raise Not_found
  | ({ packet_type = Public_Key_Packet} as lead_packet)::tl ->
    let rec loop packets = match packets with
      | [] -> []
      | ({ packet_type = Public_Subkey_Packet} as pack)::tl ->
        from_packet pack :: loop tl
      | pack :: tl -> loop tl
    in
    (from_packet lead_packet, loop tl)
  | _ -> raise Not_found
;;

(** [key_and_subkey_ids key ~get] Returns the result of applying [get] to the
    [result] of the lead key, paired with the unique results of applying get to
    the [result] of the subkeys.  The ids of the subkey won't include the ids of
    the lead key.
*)
let key_and_subkey_ids key ~get =
  let (key_result,subkey_results) = key_and_subkey_results key in
  let key_id = get key_result in
  let subkey_ids =
    List.map ~f:get subkey_results
    |! Set.of_list |! Set.remove key_id |! Set.elements
  in
  (key_id,subkey_ids)
;;

(** returns main keyid and list of subkey keyids.  The keyid is guaranteed not
    to appear among the subkey keyids, and there are no duplicates among the
    subkey keyids.
*)
let keyids_from_key ?(short=true) key =
  key_and_subkey_ids key ~get:(fun r -> shorten ~short r.keyid)
;;

(** returns main key fingerprint and list of subkey fingerprints.  The
    fingerprint is guaranteed not to appear among the subkey fingerprints, and
    there are no duplicates among the subkey fingerprints.  This list is made to
    facilitate searching by long keyid (16 digit) or fingerprint. This was in
    response to a 28-Dec-Patch to all trees of GnuPG allowing key lookup by
    short keyID (8 digit), long KeyID, or fingerprint
*)
let fps_from_key key =
  key_and_subkey_ids key ~get:(fun r -> r.fp)
;;
