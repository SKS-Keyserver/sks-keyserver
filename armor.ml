(***********************************************************************)
(* armor.ml- Conversion to and from ASCII armor                        *)
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

external crc_of_string : string -> int = "caml_crc_octets"

let base64crc input =
  let encoder = Cryptokit.Base64.encode_multiline () in
  encoder#put_string input;
  encoder#finish;
  let base64 = encoder#get_string in
  let crc = crc_of_string input in
  let encoder = Cryptokit.Base64.encode_compact () in
  encoder#put_char (char_of_int ((crc lsr 16) land 0xFF));
  encoder#put_char (char_of_int ((crc lsr 8) land 0xFF));
  encoder#put_char (char_of_int (crc land 0xFF));
  encoder#finish;
  let base64 =
    if base64.[String.length base64 - 1] <> '\n'
    then base64 ^ "\n" else base64 in
  base64 ^ "=" ^ encoder#get_string

let pubkey_armor_header = "-----BEGIN PGP PUBLIC KEY BLOCK-----"
let pubkey_armor_tail = "-----END PGP PUBLIC KEY BLOCK-----"

(* pubkey *)
let encode_pubkey key =
  let armor_header = pubkey_armor_header
  and armor_tail = pubkey_armor_tail
  and version = (sprintf "Version: SKS %s%s" Common.version Common.version_suffix)
  and hostname = (sprintf "Comment: Hostname: %s" (if String.length !Settings.hostname > 53 then String.sub !Settings.hostname 0 53 else !Settings.hostname))
  in
  let input = Key.to_string key in
  armor_header ^ "\n" ^
  version ^ "\n" ^
  hostname ^ "\n\n" ^
  base64crc input ^ "\n" ^
  armor_tail

let encode_pubkey_string keystr =
  let armor_header = pubkey_armor_header
  and armor_tail = pubkey_armor_tail
  and version = (sprintf "Version: SKS %s%s" Common.version Common.version_suffix)
  and hostname = (sprintf "Comment: Hostname: %s" (if String.length !Settings.hostname > 53 then String.sub !Settings.hostname 0 53 else !Settings.hostname))
  in
  let input = keystr in
  armor_header ^ "\n" ^
  version ^ "\n" ^
  hostname ^ "\n\n" ^
  base64crc input ^ "\n" ^
  armor_tail

let decode_crc s =
  let decoder = Cryptokit.Base64.decode () in
  decoder#put_string s;
  decoder#finish;
  let b1 = decoder#get_byte in
  let b2 = decoder#get_byte in
  let b3 = decoder#get_byte in
  b1 lsl 16 + b2 lsl 8 + b3

let eol = Str.regexp "[ \t]*\r?\n"

let decode_pubkey text =
  let decoder = Cryptokit.Base64.decode () in
  let lines = Str.split eol text in
  let rec read_adata lines = match lines with
      [] -> failwith "Error while decoding ascii-armored key: text terminated before reaching CRC sum"
    | line::tl ->
        if line.[0] = '='
        then ( (* close the decoder and return the CRC string *)
          decoder#finish;
          let crc = decode_crc (String.sub ~pos:1
                                  ~len:(String.length line - 1) line)
          and data = decoder#get_string in
          (data,crc)
        )
        else (
          decoder#put_string line;
          read_adata tl
        )
  and read_full lines = match lines with
      [] -> failwith "Error while decoding ascii-armored key:  text terminated before reaching PGP public key header line"
    | line::tl ->
        if line = pubkey_armor_header then read_block tl
        else read_full tl
  and read_block lines = match lines with
      [] -> failwith "Error while decoding ascii-armored key: text terminated before beginning of ascii block"
    | line::tl ->
        if line = "" then read_adata tl
        else read_block tl
  in
  let (data,crc) = read_full lines in
  let data_crc = crc_of_string data in
  assert (data_crc = crc);
  Key.of_string_multiple data


