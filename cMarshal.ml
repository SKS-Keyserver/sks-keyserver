(***********************************************************************)
(* cMarshal.ml - Marshaling into and out of channels                   *)
(*               (see [Channel] module)                                *)
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
module ZSet = ZZp.Set

let marshal_string cout string =
  ignore (cout:>Channel.out_channel_obj);
  cout#write_int (String.length string);
  cout#write_string string

let unmarshal_string cin =
  let length = cin#read_int in
    cin#read_string length

(*****)

let marshal_lstring cout string =
  cout#write_string string

let unmarshal_lstring length cin =
  cin#read_string length


(*****)

let rec marshal_array ~f cout array =
  cout#write_int (Array.length array);
  Array.iter ~f:(f cout) array

let rec unmarshal_array ~f cin =
  let len = cin#read_int in
  Array.init len ~f:(fun i -> f cin)

(*****)

let rec marshal_list ~f cout list =
  cout#write_int (List.length list);
  List.iter ~f:(f cout) list

let rec unmarshal_list ~f cin =
  Array.to_list (unmarshal_array ~f cin)

(*****)

let marshal_fixed_sarray cout sarray =
  let len = try String.length sarray.(0) with _ -> 0 in
  Array.iter ~f:(fun s ->
                   if String.length s <> len
                   then failwith ("Strings not same length in " ^
                                  "marshal_fixed_sarray")) sarray;
  cout#write_int len;
  marshal_array ~f:marshal_lstring cout sarray

let unmarshal_fixed_sarray cin sarray =
  let len = cin#read_int in
  unmarshal_array ~f:(unmarshal_lstring len) cin

(*****)


let marshal_bitstring cout bs =
  cout#write_int (Bitstring.num_bits bs);
  marshal_string cout (Bitstring.to_bytes_nocopy bs)

let unmarshal_bitstring cin =
  let bitlength = cin#read_int
  and string = unmarshal_string cin in
  Bitstring.of_bytes_nocopy string bitlength

(*****)

let marshal_set ~f cout set =
  let array = Array.of_list (ZSet.elements set) in
  marshal_array ~f cout array


let unmarshal_set ~f cin =
  let array = unmarshal_array ~f cin in
  ZZp.zset_of_list (Array.to_list array)

(*************************************************************)

let marshal_sockaddr cout sockaddr =
  match sockaddr with
    | Unix.ADDR_UNIX s ->
        cout#write_byte 0; marshal_string cout s
    | Unix.ADDR_INET (s,i) ->
        cout#write_byte 1;
        marshal_string cout (Unix.string_of_inet_addr s);
        cout#write_int i

let unmarshal_sockaddr cin =
  match cin#read_byte with
      0 -> Unix.ADDR_UNIX (unmarshal_string cin)
    | 1 ->
        let s = unmarshal_string cin in
        let i = cin#read_int in
        Unix.ADDR_INET (Unix.inet_addr_of_string s,i)
    | _ -> failwith "Unmarshalling failed: malformed sockaddr"

(************************************************************)

let marshal_to_string ~f x =
  let cout = Channel.new_buffer_outc 0 in
  f cout x;
  cout#contents

let unmarshal_from_string ~f s =
  let cin = new Channel.string_in_channel s 0 in
  f cin


let int_to_string x = marshal_to_string ~f:(fun cout x -> cout#write_int x) x
let int_of_string s = unmarshal_from_string ~f:(fun cin -> cin#read_int) s

