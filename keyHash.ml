(***********************************************************************)
(* keyHash.ml - Sorts key and generates MD5 hash of sorted key         *)
(*              Note that hash should not depend on whether old or     *)
(*              new-style packets are used, although for nested        *)
(*              packets, packet format will make a difference.         *)
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

open Packet
open Printf

let hash_bytes = 16

let packet_cmp p1 p2 =
  let c = compare p1.content_tag p2.content_tag in
  if c <> 0 then c
  else compare p1.packet_body p2.packet_body

(* takes a key and dumps all of its contents into one long string *)
let concat key =
  let length = List.fold_left
                 ~f:(fun sum p -> sum + 4 + p.packet_length)
                 ~init:0 key
  in
  let bufc = Channel.new_buffer_outc length in
  List.iter ~f:(fun p ->
                  bufc#write_int p.content_tag ;
                  bufc#write_int p.packet_length;
                  bufc#write_string p.packet_body)
    key;
  bufc#contents

let sort key =
  List.sort ~cmp:packet_cmp key

let hash key =
  let keystring = concat (sort key) in
  let hash = Digest.string keystring in
  (hash : string)


let hexify s = Utils.hexstring s

let hexchar_to_int c =
  let ic = int_of_char c in
  if ic >= int_of_char '0' && ic <= int_of_char '9' then
    ic - int_of_char '0'
  else (
    if not (ic <= int_of_char 'F' && ic >= int_of_char 'A')
    then failwith "char out of range for hex conversion";
    ic - int_of_char 'A' + 10
  )

let dehexify s =
  let s = String.uppercase s in
  let ns = String.create (String.length s / 2) in (* new string *)
  for i = 0 to String.length ns - 1 do
    let first = hexchar_to_int s.[2 * i]
    and second = hexchar_to_int s.[2 * i + 1]
    in
    ns.[i] <- char_of_int ((first lsl 4) + second)
  done;
  ns
