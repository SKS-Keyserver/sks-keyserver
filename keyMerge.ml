(***********************************************************************)
(* keyMerge.ml -  Logic for merging PGP keys with the same public key  *)
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
open Common
open Packet

module Set = PSet.Set
module Map = PMap.Map

exception Unparseable_packet_sequence

(** This is my understanding of the grammar of allowable public keys:

{[   ATOMS = v3_pubkey v4_pubkey signature pubsubkey uid e]}

   The above correspond to packet types, except for e, which corresponds to
   the empty string.

   Here's the grammar:

{[      KEY := V3 | V4
      V3 := v3_pubkey SIGLIST UIDLIST
      V4 := v4_pubkey SIGLIST UIDLIST SUBKEYLIST
      SIGLIST := e | signature SIGLIST
      UIDLIST := e | UID UIDLIST
      UID := uid SIGLIST | uid
      SUBKEYLIST := e | SUBKEY SUBKEYLIST
      SUBKEY := subkey SIGLIST ]}

(shouldn't the last one be:
  {[SUBKEY := subkey signature SIGLIST]}
 since there must be at least one signature?)

   My only purpose in doing this parsing is to allow for the proper merging
   of two public keys.
   To merge two keys, I join the SIGLISTs and UIDLISTs and SUBKEYLISTs.
     + Merging SIGLISTs is straightforward: just concatentate the lists and drop
       duplicates.
     + Merging UIDLISTs and SUBKEYLISTs is somewhat more complicated.  I
       join siglists corresponding to the same UID.

   The current implementation explicitly distinguishes between v3 and v4
   keys, which really it doesn't need to do as it presently stands.  But if a
   fuller handler of revocation becomes necessary, then distinguishing
   between the two may be necessary.

   There is no special handling of revocations --- I don't check if they're
   valid, and multiple revocations can pop up.
*)


(*******************************************************************)
(* Types for representing the structure of a key *)

type sigpair = packet * packet list

type pkey = { key : packet;
              selfsigs: packet list; (* revocations only in v3 keys *)
              uids: sigpair list;
              subkeys: sigpair list;
            }

let packets_equal p1 p2 = p1 = p2

(*******************************************************************)
(** Code for flattening out the above structure back to the original key *)

let rec flatten_sigpair_list list = match list with
    [] -> []
  |  (pack,sigs)::tl -> pack :: (sigs @ flatten_sigpair_list tl)

let flatten key =
  key.key :: List.concat [ key.selfsigs;
                           flatten_sigpair_list key.uids;
                           flatten_sigpair_list key.subkeys ]


(************************************************************)

let print_pkey key =
  printf "%d selfsigs, %d uids, %d subkeys\n"
    (List.length key.selfsigs)
    (List.length key.uids)
    (List.length key.subkeys)


(*******************************************************************)

let get_version packet =
  match packet.packet_type with
      Public_Key_Packet -> int_of_char packet.packet_body.[0]
    | Signature_Packet -> int_of_char packet.packet_body.[0]
    | _ -> raise Not_found

let key_to_stream key =
  let ptype_list = List.map ~f:(fun pack -> (pack.packet_type,pack)) key in
  Stream.of_list ptype_list




(*******************************************************************)
(*** Key Parsing ***************************************************)
(*******************************************************************)

let rec parse_keystr = parser
  | [< '(Public_Key_Packet,p) ; s >] ->
      match get_version p with
        | 4 ->
            (match s with parser [< selfsigs = siglist;
                                    uids = uidlist;
                                    subkeys = subkeylist;
                                 >]
                 -> { key = p;
                      selfsigs = selfsigs;
                      uids = uids;
                      subkeys = subkeys;
                    })
        | 2 | 3 ->
            (match s with parser [< revocations = siglist;
                                    uids = uidlist;
                                 >] ->
               { key = p ;
                 selfsigs = revocations;
                 uids = uids;
                 subkeys = [];
               })
        | _ -> failwith "Unexpected key packet version number"
and siglist = parser
  | [< '(Signature_Packet,p); tl = siglist >] -> p::tl
  | [< >] -> []
and uidlist = parser
  | [< '(User_ID_Packet,p); sigs = siglist; tl = uidlist >] ->
      (p,sigs)::tl
  | [< '(User_Attribute_Packet,p); sigs = siglist; tl = uidlist >] ->
      (p,sigs)::tl
      (*
      (p,sigs)::(match s with parser
                    | [< '(User_ID_Packet,p); sigs = siglist; tl = uidlist >] ->
                       (p,sigs)::tl
                   | [< >] -> [])
      *)
  | [< >] -> []
and subkeylist = parser
  | [< '(Public_Subkey_Packet,p); sigs = siglist; tl = subkeylist >] ->
      (p,sigs)::tl
  | [< >] -> []

(*******************************************************************)
(*** Key Merging Code  *********************************************)
(*******************************************************************)

let set_of_list list = List.fold_left ~init:Set.empty list
                         ~f:(fun set x -> Set.add x set)

let merge_sigpairs pairs =
  let map =
    List.fold_left pairs
      ~f:(fun map (pack,sigs) ->
            try
              let old_sigs = Map.find pack map in
              (* If front packet is already there, add in new sigs,
                 discarding duplicates *)
              Map.add ~key:pack ~data:(Utils.dedup (old_sigs @ sigs)) map
            with
                (* otherwise, add in data by itself *)
                Not_found -> Map.add ~key:pack ~data:sigs map)
      ~init:Map.empty
  in
  Map.fold ~f:(fun ~key:pack ~data:sigs list -> (pack,sigs)::list) map ~init:[]

let merge_sigpair_lists l1 l2 =
  merge_sigpairs (l1 @ l2)

(*******************************************************************)

let merge_pkeys key1 key2 =
  if not (packets_equal key1.key key2.key)
  then None (* merge can only work if keys are the same *)
  else
    Some { key = key1.key;
           selfsigs = Utils.dedup (key1.selfsigs @ key2.selfsigs);
           (* this might be wrong.  Must the revocations
              be separated out to go before the other self
              signatures? *)
           uids = merge_sigpair_lists key1.uids key2.uids;
           subkeys = merge_sigpair_lists key1.subkeys key2.subkeys;
         }

(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

let key_to_pkey key =
  try
    let keystream = key_to_stream key in
    let pkey = parse_keystr keystream in
    Stream.empty keystream;
    pkey
  with
      Stream.Failure | Stream.Error _ ->
        raise Unparseable_packet_sequence


let merge key1 key2 =
  try
    let pkey1 = key_to_pkey key1
    and pkey2 = key_to_pkey key2 in
    let mkey = merge_pkeys pkey1 pkey2 in
    apply_opt ~f:flatten mkey
  with
      Unparseable_packet_sequence -> None

let dedup_sigpairs pairs =
  let map =
    List.fold_left pairs ~init:Map.empty
      ~f:(fun map (pack,sigs) ->
            try
              let old_sigs = Map.find pack map in
              Map.add ~key:pack ~data:(Utils.dedup (sigs @ old_sigs)) map
            with
                Not_found -> Map.add ~key:pack ~data:sigs map
         )
  in
  Map.to_alist map


let dedup_pkey pkey =
  { pkey with
      selfsigs = Utils.dedup pkey.selfsigs;
      uids = dedup_sigpairs pkey.uids;
      subkeys = dedup_sigpairs pkey.subkeys;
  }

let dedup_key key = flatten (dedup_pkey (key_to_pkey key))

let parseable key =
  try ignore (key_to_pkey key); true
  with Unparseable_packet_sequence -> false
