(***********************************************************************)
(* packet.ml -  Type definitions and simple functions related to PGP   *)
(*              packets                                                *)
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

type ptype = | Reserved
             | Public_Key_Encrypted_Session_Key_Packet
             | Signature_Packet
             | Symmetric_Key_Encrypted_Session_Key_Packet
             | One_Pass_Signature_Packet
             | Secret_Key_Packet
             | Public_Key_Packet
             | Secret_Subkey_Packet
             | Compressed_Data_Packet
             | Symmetrically_Encrypted_Data_Packet
             | Marker_Packet
             | Literal_Data_Packet
             | Trust_Packet
             | User_ID_Packet
             | User_Attribute_Packet
             | Sym_Encrypted_and_Integrity_Protected_Data_Packet
             | Modification_Detection_Code_Packet
             | Public_Subkey_Packet
             | Private_or_Experimental_ptype
             | Unexpected_ptype

type packet = { content_tag: int;
                packet_type: ptype;
                packet_length: int;
                packet_body: string;
              }

type sigsubpacket =
    { ssp_length: int;
      ssp_type: int;
      ssp_body: string;
    }

let ssp_type_to_string i = match i with
  | 2 -> "signature creation time"
  | 3 -> "signature expiration time"
  | 4 -> "exportable certification"
  | 5 -> "trust signature"
  | 6 -> "regular expression"
  | 7 -> "revocable"
  | 9 -> "key expiration time"
  | 10 -> "placeholder for backward compatibility"
  | 11 -> "preferred symmetric algorithms"
  | 12 -> "revocation key"
  | 16 -> "issuer key ID"
  | 20 -> "notation data"
  | 21 -> "preferred hash algorithms"
  | 22 -> "preferred compression algorithms"
  | 23 -> "key server preferences"
  | 24 -> "preferred key server"
  | 25 -> "primary user id"
  | 26 -> "policy URL"
  | 27 -> "key flags"
  | 28 -> "signer's user id"
  | 29 -> "reason for revocation"
  | 30 -> "features"
  | 31 -> "signature target"
  | 32 -> "embedded signature"
  | x when x >= 100 && x <= 110 -> "internal or user-defined"
  | _ -> failwith "Unexpected sigsubpacket type"

type key = packet list

let sigtype_to_string sigtype = match sigtype with
  | 0x00 -> "signature of binary document"
  | 0x01 -> "signature of canonical text document"
  | 0x02 -> "Standalone signature"
  | 0x10 -> "Generic certification of a User ID and Public Key packet"
  | 0x11 -> "Persona certification of a User ID and Public Key packet"
  | 0x12 -> "Casual certification of a User ID and Public Key packet"
  | 0x13 -> "Positive certification of a User ID and Public Key packet"
  | 0x18 -> "Subkey Binding Signature"
  | 0x19 -> "Primary Key Binding Signature"
  | 0x1F -> "Signature directly on a key"
  | 0x20 -> "Key revocation signature"
  | 0x28 -> "Subkey revocation signature"
  | 0x30 -> "Certification revocation signature"
  | 0x40 -> "Timestamp signature"
  | 0x50 -> "Third-Party Confirmation signature."
  | _ -> "UNEXPECTED SIGTYPE"

let content_tag_to_ptype tag = match tag with
    | 0 -> Reserved
    | 1 -> Public_Key_Encrypted_Session_Key_Packet
    | 2 -> Signature_Packet
    | 3 -> Symmetric_Key_Encrypted_Session_Key_Packet
    | 4 -> One_Pass_Signature_Packet
    | 5 -> Secret_Key_Packet
    | 6 -> Public_Key_Packet
    | 7 -> Secret_Subkey_Packet
    | 8 -> Compressed_Data_Packet
    | 9 -> Symmetrically_Encrypted_Data_Packet
    | 10 -> Marker_Packet
    | 11 -> Literal_Data_Packet
    | 12 -> Trust_Packet
    | 13 -> User_ID_Packet
    | 14 -> Public_Subkey_Packet
    | 17 -> User_Attribute_Packet
    | 18 -> Sym_Encrypted_and_Integrity_Protected_Data_Packet
    | 19 -> Modification_Detection_Code_Packet
    | 60 | 61 | 62 | 63 -> Private_or_Experimental_ptype
    | _ -> Unexpected_ptype

let ptype_to_string ptype = match ptype with
    | Reserved                                   -> "Reserved - a packet tag must not have this value"
    | Public_Key_Encrypted_Session_Key_Packet    -> "Public-Key Encrypted Session Key Packet"
    | Signature_Packet                           -> "Signature Packet"
    | Symmetric_Key_Encrypted_Session_Key_Packet -> "Symmetric-Key Encrypted Session Key Packet"
    | One_Pass_Signature_Packet                  -> "One-Pass Signature Packet"
    | Secret_Key_Packet                          -> "Secret Key Packet"
    | Public_Key_Packet                          -> "Public Key Packet"
    | Secret_Subkey_Packet                       -> "Secret Subkey Packet"
    | Compressed_Data_Packet                     -> "Compressed Data Packet"
    | Symmetrically_Encrypted_Data_Packet        -> "Symmetrically Encrypted Data Packet"
    | Marker_Packet                              -> "Marker Packet"
    | Literal_Data_Packet                        -> "Literal Data Packet"
    | Trust_Packet                               -> "Trust Packet"
    | User_ID_Packet                             -> "User ID Packet"
    | Public_Subkey_Packet                       -> "Public Subkey Packet"
    | User_Attribute_Packet                      -> "User Attribute Packet"
    | Sym_Encrypted_and_Integrity_Protected_Data_Packet ->
        "Sym Encrypted and Integrity Protected Data Packet"
    | Modification_Detection_Code_Packet         -> "Modification Detection Code Packet"
    | Private_or_Experimental_ptype              -> "Private or Experimental Values"
    | Unexpected_ptype                           -> "Unexpected value"

type mpi = { mpi_bits: int;
             mpi_data: string;
           }

let pubkey_algorithm_string i =  match i with
  | 1 -> "RSA (Encrypt or Sign)"
  | 2 -> "RSA Encrypt-Only"
  | 3 -> "RSA Sign-Only"
  | 16 -> "Elgamal (Encrypt-Only), see [ELGAMAL]"
  | 17 -> "DSA (Digital Signature Standard)"
  | 18 -> "ECDH (ECC)" (* RFC 6637 *)
  | 19 -> "ECDSA (ECC)" (* RFC 6637 *)
  | 20 -> "Elgamal (Encrypt or Sign)"
  | 21 -> "Reserved for Diffie-Hellman (X9.42) as defined for IETF-S/MIME"
  | 22 -> "EdDSA"
  | x when x >= 100 && x <= 110 -> "Private/Experimental algorithm."
  | _ -> "Unknown Public Key Algorithm"


type pubkeyinfo =
    { pk_version: int;
      pk_ctime: int64;
      pk_expiration: int option;
      pk_alg: int;
      pk_keylen: int;
    }



type sigtype = | Signature_of_a_binary_document
               | Signature_of_a_canonical_text_document
               | Standalone_signature
               | Generic_certification_of_a_User_ID_and_Public_Key_packet
               | Persona_certification_of_a_User_ID_and_Public_Key_packet
               | Casual_certification_of_a_User_ID_and_Public_Key_packet
               | Positive_certification_of_a_User_ID_and_Public_Key_packet
               | Subkey_Binding_Signature
               | Signature_directly_on_a_key
               | Key_revocation_signature
               | Subkey_revocation_signature
               | Certification_revocation_signature
               | Timestamp_signature
               | Unexpected_sigtype

type v3sig =
    { v3s_sigtype: int;
      v3s_ctime: int64;
      v3s_keyid: string;
      v3s_pk_alg: int;
      v3s_hash_alg: int;
      v3s_hash_value: string;
      v3s_mpis: mpi list;
    }

type v4sig =
    { v4s_sigtype: int;
      v4s_pk_alg: int;
      v4s_hashed_subpackets: sigsubpacket list;
      v4s_unhashed_subpackets: sigsubpacket list;
      v4s_hash_value: string;
      v4s_mpis: mpi list;
    }

type signature = V3sig of v3sig | V4sig of v4sig

let int_to_sigtype byte =
  match byte with
  | 0x00 -> Signature_of_a_binary_document
  | 0x01 -> Signature_of_a_canonical_text_document
  | 0x02 -> Standalone_signature
  | 0x10 -> Generic_certification_of_a_User_ID_and_Public_Key_packet
  | 0x11 -> Persona_certification_of_a_User_ID_and_Public_Key_packet
  | 0x12 -> Casual_certification_of_a_User_ID_and_Public_Key_packet
  | 0x13 -> Positive_certification_of_a_User_ID_and_Public_Key_packet
  | 0x18 -> Subkey_Binding_Signature
  | 0x1F -> Signature_directly_on_a_key
  | 0x20 -> Key_revocation_signature
  | 0x28 -> Subkey_revocation_signature
  | 0x30 -> Certification_revocation_signature
  | 0x40 -> Timestamp_signature
  | _ ->    Unexpected_sigtype

let content_tag_to_string tag =
  ptype_to_string (content_tag_to_ptype tag)

let print_packet packet =
  printf "%s\n" (ptype_to_string packet.packet_type);
  printf "Length: %d\n" packet.packet_length;
  if packet.packet_type = User_ID_Packet
  then (print_string packet.packet_body; print_string "\n")

(** write out new-style packet *)
let write_packet_new packet cout =
  (* specify new packet format *)
  cout#write_byte (packet.content_tag lor 0xC0);
  cout#write_byte 0xFF;
  cout#write_int packet.packet_length;
  cout#write_string packet.packet_body

let pk_alg_to_ident i = match i with
  | 1 -> "R"  (* RSA sign and encrypt *)
  | 2 -> "r"  (* RSA encrypt *)
  | 3 -> "s"  (* RSA sign *)
  | 16 -> "g"  (* ElGamal encrypt *)
  | 17 -> "D"  (* DSA *)
  | 18 -> "e"  (* ECDH *)
  | 19 -> "E"  (* ECDSA *)
  | 20 -> "G"  (* ElGamal sign and encrypt *)
  | 22 -> "E"  (* EdDSA *)
  | _  -> "?"  (* NoClue *)

(** writes out packet, using old-style packets when possible *)
let write_packet_old packet cout =
  if packet.content_tag >= 16
  then (* write new-style packet *)
    write_packet_new packet cout
  else (* write old-style packet *)
    begin
      let length_type =
        if packet.packet_length < 256 then 0
        else if packet.packet_length < 65536 then 1
        else 2
      in
      cout#write_byte ((packet.content_tag lsl 2) lor 0x80 lor length_type);
      (match length_type with
           0 -> cout#write_byte packet.packet_length
         | 1 ->
             cout#write_byte ((packet.packet_length lsr 8) land 0xFF);
             cout#write_byte (packet.packet_length land 0xFF);
         | 2 ->
             cout#write_byte ((packet.packet_length lsr 24) land 0xFF);
             cout#write_byte ((packet.packet_length lsr 16) land 0xFF);
             cout#write_byte ((packet.packet_length lsr 8) land 0xFF);
             cout#write_byte (packet.packet_length land 0xFF);
         | _ ->
             failwith "Packet.write_packet_old: Bug -- bad packet length"
      );
      cout#write_string packet.packet_body
    end


let write_packet = write_packet_old
