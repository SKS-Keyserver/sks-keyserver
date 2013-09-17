(***********************************************************************)
(* key.ml - Basic key-related operations                               *)
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
module Set = PSet.Set

exception Bug of string


(*************************************************************)

let rec pos_next_rec ps partial =
  match SStream.peek ps with
      None -> Some (List.rev partial)
    | Some (_,packet) ->
        if packet.packet_type = Public_Key_Packet
        then Some (List.rev partial)
        else (
          SStream.junk ps;
          pos_next_rec ps (packet::partial)
        )

let pos_next ps =
  match SStream.peek ps with
      None -> None
    | Some (pos,pack) ->
        SStream.junk ps;
        match pos_next_rec ps [pack] with
            Some key -> Some (pos,key)
          | None -> None

let pos_get ps =
  match pos_next ps with
      None -> raise Not_found
    | Some key -> key

let pos_next_of_channel cin =
  let ps =
    SStream.make (fun () -> (try Some (ParsePGP.offset_read_packet cin)
                             with End_of_file -> None))
  in
  (fun () -> pos_next ps)

let pos_get_of_channel cin =
  let ps =
    SStream.make (fun () -> (try Some (ParsePGP.offset_read_packet cin)
                             with End_of_file -> None))
  in
  (fun () -> pos_get ps)

(*************************************************************)

let rec next_rec ps partial =
  match SStream.peek ps with
      None -> Some (List.rev partial)
    | Some packet ->
        if packet.packet_type = Public_Key_Packet
        then Some (List.rev partial)
        else (
          SStream.junk ps;
          next_rec ps (packet::partial)
        )

let next ps =
  match SStream.peek ps with
      None -> None
    | Some pack ->
        SStream.junk ps;
        next_rec ps [pack]

let get ps =
  match next ps with
      None -> raise Not_found
    | Some key -> key

let next_of_channel cin =
  let ps =
    SStream.make (fun () -> (try Some (ParsePGP.read_packet cin)
                             with End_of_file -> None))
  in
  (fun () -> next ps)

let get_of_channel cin =
  let ps =
    SStream.make (fun () -> (try Some (ParsePGP.read_packet cin)
                             with End_of_file -> None))
  in
  (fun () -> get ps)


(*************************************************************)

let rec get_ids key = match key with
    [] -> []
  | packet::tail ->
      if packet.packet_type = User_ID_Packet
      then packet.packet_body::(get_ids tail)
      else get_ids tail

(*************************************************************)

let write key cout =
  List.iter ~f:(fun packet -> write_packet packet cout) key

let to_string key =
  let cout = Channel.new_buffer_outc 0 in
  write key cout;
  cout#contents

let of_string keystr =
  let cin = new Channel.string_in_channel keystr 0 in
  match next_of_channel cin () with
      None -> raise (Bug "key should have appeared")
    | Some key -> key

let of_string_multiple keystr =
  let cin = new Channel.string_in_channel keystr 0 in
  let next = next_of_channel cin in
  let rec loop () =
    match next () with
        None -> []
      | Some key -> key::(loop ())
  in
  loop ()

let to_string_multiple keys =
  let cout = Channel.new_buffer_outc 0 in
  List.iter ~f:(fun key -> write key cout) keys;
  cout#contents

(*************************************************************)

let to_words key =
  let userids = get_ids key in
  let wordsets = List.map ~f:Utils.extract_word_set userids in
  Set.elements (List.fold_left ~init:Set.empty ~f:Set.union
                  wordsets)
