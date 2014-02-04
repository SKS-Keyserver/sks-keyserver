(***********************************************************************)
(* parsePGP.ml                                                         *)
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
open Printf

exception Overlong_mpi
exception Partial_body_length of int

(********************************************************)

(** parse new-style packet length *)
let parse_new_packet_length cin =
  let byte1 = cin#read_byte in
  if byte1 <= 191 then byte1  (* one-octet length *)
  else if byte1 <= 223  then (* two-octet length *)
    let byte2 = cin#read_byte in
    (byte1 - 192) lsl 8 + byte2 + 192
  else if byte1 = 255 then (* five-octet length *)
    let byte2 = cin#read_byte in
    let byte3 = cin#read_byte in
    let byte4 = cin#read_byte in
    let byte5 = cin#read_byte in
    (byte2 lsl 24) lor (byte3 lsl 16) lor (byte4 lsl 8) lor byte5
  else (* partial body length *)
    raise (Partial_body_length (1 lsl (byte1 land 0x1f)))

(********************************************************)

let read_packet cin =
  let packet_tag = cin#read_byte in
  if ((packet_tag lsr 7) land 1 <> 1)
  then failwith (sprintf "Bit 7 of packet tag was not 1 as expected: %x"
                   packet_tag);
  match (packet_tag lsr 6) land 1 with

      0 -> (* old format *)
        let content_tag = (packet_tag land 0b111100) lsr 2
        and length_type = packet_tag land 0b11
        in
        (match length_type with
             0 | 1 | 2 ->
               let length_length = 1 lsl length_type in
               let length_str = cin#read_string length_length in
               let length = Utils.int_from_bstring length_str
                              ~pos:0 ~len:length_length in
               { content_tag = content_tag;
                 packet_type = content_tag_to_ptype content_tag;
                 packet_length = length;
                 packet_body = cin#read_string length;
               }

           | 3 -> (* indeterminate length header --- extends to end of file *)
               failwith "Unexpected indeterminate length packet"
           | _ ->
               failwith "Unexpected length type"
        )

    | 1 -> (* new_format *)
        let content_tag = packet_tag land 0b111111 in
        let length = parse_new_packet_length cin in
        { (* packet_tag = packet_tag; *)
          content_tag = content_tag;
          packet_type = content_tag_to_ptype content_tag;
          packet_length = length;
          packet_body = cin#read_string length;
        }

    | _ -> raise (Bug "ParsePGP.read_packet: expected 0/1 value")


(********************************************************)

let offset_read_packet cin =
  let offset = LargeFile.pos_in cin#inchan in
  let packet = read_packet cin in
  (offset,packet)

(********************************************************)

let offset_length_read_packet cin =
  let offset = pos_in cin#inchan in
  let packet = read_packet cin in
  let final_offset = pos_in cin#inchan in
  (packet,offset,final_offset - offset)

(********************************************************)

let read_mpi cin =
  let byte1 = cin#read_byte in
  try
    let byte2 = cin#read_byte in
    let length = (byte1 lsl 8) + byte2 in
    let data = cin#read_string
                 ((length + 7)/8)
    in
    { mpi_bits = length; mpi_data = data }
  with
      End_of_file -> raise Overlong_mpi

(********************************************************)

let read_mpis cin =
  let rec loop list =
    match (try (Some (read_mpi cin))
           with End_of_file -> None)
    with
      | Some mpi -> loop (mpi::list)
      | None -> List.rev list
  in
  loop []

(********************************************************)

(* RFC6637:
   The following algorithm-specific packets are added to Section 5.5.2
   of [RFC4880], "Public-Key Packet Formats", to support ECDH and ECDSA.
 *)

(* OIDs defined in 11. ECC Curve OID of RFC6637 *)
let oid_to_psize oid =
   let psize = match oid with
     | "\x2b\x81\x04\x00\x23" -> 521         		(* nistp521 *)
     | "\x2b\x81\x04\x00\x22" -> 384         		(* nistp384 *)
     | "\x2a\x86\x48\xce\x3d\x03\x01\x07" -> 256   	(* nistp256 *)
     | "\x2b\x24\x03\x03\x02\x08\x01\x01\x07" -> 256 	(* brainpoolP256r1 *)
     | "\x2b\x24\x03\x03\x02\x08\x01\x01\x0b" -> 384 	(* brainpoolP384r1 *)
     | "\x2b\x24\x03\x03\x02\x08\x01\x01\x0d" -> 512 	(* brainpoolP512r1 *)
     | "\x2b\x81\x04\x00\x0a" -> 256         		(* secp256k1 *)
     | _ -> failwith "Unknown OID"
   in
   psize


let parse_ecdh_pubkey cin =
   let length = cin#read_int_size 1 in
   let oid = cin#read_string length in
   let mpi = read_mpi cin in
   let kdf_length = cin#read_int_size 1 in
   let kdf_res = cin#read_int_size 1 in
   let kdf_hash = cin#read_int_size 1 in
   let kdf_algid = cin#read_int_size 1 in
   plerror 10 "KDF_length: %d, KDF_res %d hash %d algid %d" kdf_length kdf_res kdf_hash kdf_algid;
   let psize = oid_to_psize oid
   in
   (mpi, psize)

 let parse_ecdsa_pubkey cin =
   let length = cin#read_int_size 1 in
   let oid = cin#read_string length in
   let psize = oid_to_psize oid
   in
   psize

let parse_pubkey_info packet =
  let cin = new Channel.string_in_channel packet.packet_body 0 in
  let version = cin#read_byte in
  let creation_time = cin#read_int64_size 4 in
  let (algorithm,mpi,expiration, psize) =
    match version with
      | 4 ->
      let algorithm = cin#read_byte in
      let (tmpmpi, tmpsize) =  match algorithm with
        | 18 -> parse_ecdh_pubkey cin
        | 19 -> ( {mpi_bits = 0; mpi_data = ""}, (parse_ecdsa_pubkey cin))
        | _ -> ( {mpi_bits = 0; mpi_data = ""} , -1 )
      in
      let mpis = match algorithm with
       | 18 -> tmpmpi
       | _ -> let mmpis = read_mpis cin in List.hd mmpis
      in
      (algorithm,mpis,None, tmpsize)
      | 2 | 3 ->
      let expiration = cin#read_int_size 2 in
      let algorithm = cin#read_byte in
      let mpis = read_mpis cin in
      let mpi = List.hd mpis in
      (algorithm,mpi,Some expiration, -1)
      | _ -> failwith (sprintf "Unexpected pubkey version: %d" version)
  in
  { pk_version = version;
    pk_ctime = creation_time;
    pk_expiration = (match expiration with Some 0 -> None | x -> x);
    pk_alg = algorithm;
    pk_keylen = (match algorithm with |18|19 -> psize | _ -> mpi.mpi_bits);
  }

(********************************************************)


(** Parsing of signature subpackets *)

(** parse sigsubpacket length *)
let parse_sigsubpacket_length cin =
  let byte1 = cin#read_byte in
  if byte1 < 192 then byte1 (* one octet length *)
  else if byte1  < 255 then
    let byte2 = cin#read_byte in
    ((byte1 - 192) lsl 8) + (byte2) + 192
  else if byte1 = 255 then (* five-octet length *)
    let byte2 = cin#read_byte in
    let byte3 = cin#read_byte in
    let byte4 = cin#read_byte in
    let byte5 = cin#read_byte in
    (byte2 lsl 24) lor (byte3 lsl 16) lor (byte4 lsl 8) lor byte5
  else
    failwith "Unable to parse sigsubpacket length"

let read_sigsubpacket cin =
  let length = parse_sigsubpacket_length cin in
  let ssp_type = cin#read_byte land 0x7f in
  let body = cin#read_string (length - 1) in
  { ssp_length = length - 1;
    ssp_type = ssp_type;
    ssp_body = body;
  }

let get_hashed_subpacket_string cin =
  let version = cin#read_byte in
  if version <> 4 then
    failwith "Attempt to parse non-v4 signature as v4 signature";
  let _sigtype = cin#read_byte in
  let _key_alg = cin#read_byte in
  let _hash_alg = cin#read_byte in
  let hashed_subpacket_count = cin#read_int_size 2 in
  (* now we can start reading the hashed sub-packets *)
  cin#read_string hashed_subpacket_count

(** return list of signature sub-packets *)
let read_subpackets cin length =
  let subpacket_string = cin#read_string length in
  let cin = new Channel.string_in_channel subpacket_string 0 in
  let rec loop list =
    match (try Some (read_sigsubpacket cin)
           with End_of_file -> None)
    with
      | Some subpack -> loop (subpack::list)
      | None -> List.rev list
  in
  loop []

let parse_signature packet =
  let cin = new Channel.string_in_channel packet.packet_body 0 in
  let version = cin#read_byte in
  match version with

    | 2 | 3 ->
        cin#skip 1; (* length packet which must be 5 *)
        let sigtype = cin#read_byte in
        let ctime = cin#read_int64_size 4 in
        let keyid = cin#read_string 8 in
        let pk_alg = cin#read_byte in
        let hash_alg = cin#read_byte in
        let hash_value = cin#read_string 2 in
        let mpis = read_mpis cin in
        V3sig { v3s_sigtype = sigtype;
                v3s_ctime = ctime;
                v3s_keyid = keyid;
                v3s_pk_alg = pk_alg;
                v3s_hash_alg = hash_alg;
                v3s_hash_value = hash_value;
                v3s_mpis = mpis;
              }

    | 4 ->
        let sigtype = cin#read_byte in
        let pk_alg = cin#read_byte in
        let _hash_alg = cin#read_byte in

        let hashed_subpacket_bytes = cin#read_int_size 2 in
        let hashed_subpackets = read_subpackets cin hashed_subpacket_bytes in

        let unhashed_subpacket_bytes = cin#read_int_size 2 in
        let unhashed_subpackets = read_subpackets cin unhashed_subpacket_bytes in

        let hash_value = cin#read_string 2 in
        let mpis = read_mpis cin in
        V4sig { v4s_sigtype = sigtype;
                v4s_pk_alg = pk_alg;
                v4s_hashed_subpackets = hashed_subpackets;
                v4s_unhashed_subpackets = unhashed_subpackets;
                v4s_hash_value = hash_value;
                v4s_mpis = mpis;
              }


    | _ -> failwith (sprintf "Unexpected signature version: %d" version)


let ssp_ctime_id = 2
let ssp_exptime_id = 3
let ssp_keyexptime_id = 9

let int32_of_string s =
  let cin = new Channel.string_in_channel s 0 in
  cin#read_int32

let int64_of_string s =
  let cin = new Channel.string_in_channel s 0 in
  cin#read_int64_size (String.length s)

let get_key_exptimes sign = match sign with
  | V3sig sign ->
      (Some sign.v3s_ctime, None)
  | V4sig sign ->
      let hashed_subpackets = sign.v4s_hashed_subpackets in
      let (ctime,exptime_delta) =
        List.fold_left hashed_subpackets ~init:(None,None)
          ~f:(fun (ctime,exptime) ssp ->
                if ssp.ssp_type = ssp_ctime_id && ssp.ssp_length = 4 then
                  (Some (int64_of_string ssp.ssp_body),exptime)
                else if ssp.ssp_type = ssp_keyexptime_id && ssp.ssp_length = 4 then
                  (ctime,Some (int64_of_string ssp.ssp_body))
                else
                  (ctime,exptime)
             )
      in
      match exptime_delta with
        | None -> (None,None)
        | Some _ -> (ctime,exptime_delta)


let get_times sign = match sign with
  | V3sig sign ->
      (Some sign.v3s_ctime, None)
  | V4sig sign ->
      let hashed_subpackets = sign.v4s_hashed_subpackets in
      let (ctime,exptime_delta) =
        List.fold_left hashed_subpackets ~init:(None,None)
          ~f:(fun (ctime,exptime) ssp ->
                if ssp.ssp_type = ssp_ctime_id && ssp.ssp_length = 4 then
                  (Some (int64_of_string ssp.ssp_body),exptime)
                else if ssp.ssp_type = ssp_exptime_id && ssp.ssp_length = 4 then
                  (ctime,Some (int64_of_string ssp.ssp_body))
                else
                  (ctime,exptime)
             )
      in
      match (ctime,exptime_delta) with
        | (Some x,None) -> (Some x,None)
        | (None,_) -> (None,None)
        | (Some x,Some y) -> (Some x,Some (Int64.add x y))
