(***********************************************************************)
(* recoverList.ml - Code for managing reconserver's recover list, i.e. *)
(*                  the list of keys that need to be recovered from    *)
(*                  other hosts.                                       *)
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

(** Queue of bundles of hashes to be recovered*)
type recover_element = string list * Unix.sockaddr

let hash_bundle_size = !Settings.http_fetch_size
let recover_list = (Queue.create () : recover_element Queue.t)

let gossip_disabled_var = ref false

let gossip_disabled () =
  not (Queue.is_empty recover_list) || !gossip_disabled_var
let disable_gossip () =
  plerror 5 "Disabling gossip";
  gossip_disabled_var := true
let enable_gossip () =
  plerror 5 "Enabling gossip";
  gossip_disabled_var := false


(******************************************************)

let rec n_split list n = match (n,list) with
    (0,_) | (_,[]) -> ([],list)
  | (_,hd::tl) ->
      let (first,rest) = n_split tl (n - 1) in
      (hd::first,rest)

let size_split list size =
  let rec loop list accum =
    match n_split list size with
      | ([],[]) -> List.rev accum
      | (first,rest) -> loop rest (first::accum)
  in
  loop list []

let print_hashes source hashes  =
  if List.length hashes = 0
  then plerror 4 "No hashes recovered from %s" source

  else if List.length hashes <= 10 then (
    plerror 3 "%d hashes recovered from %s" (List.length hashes) source;
    List.iter hashes
      ~f:(fun hash -> plerror 3 "\t%s" (KeyHash.hexify hash));
  ) else
    plerror 3 "%d hashes recovered from %s" (List.length hashes) source

(** converts a list of elements of ZZp to a sorted list of hashes *)
let hashconvert elements =
  let hashes = List.rev_map ~f:ZZp.to_bytes elements in
  let hashes = List.rev_map ~f:(fun hash -> RMisc.truncate hash
                              KeyHash.hash_bytes) hashes in
  let hashes = List.sort ~cmp:compare hashes in
  hashes

(** Dumps the hashes associated with the difference set to the named file *)
let log_diffs log_fname hashes =
  if !Settings.log_diffs then
    begin
      let log_fname = Filename.concat !Settings.basedir log_fname in
      let file = open_out log_fname in
      protect ~f:(fun () -> List.iter hashes
          ~f:(fun h -> fprintf file "%s\n" (KeyHash.hexify h)))
        ~finally:(fun () -> close_out file)
    end

let update_recover_list results partner_http_addr  =
  let hashes = hashconvert results in
  let bundles = size_split hashes hash_bundle_size in
  List.iter bundles ~f:(fun bundle ->
                          Queue.add (bundle,partner_http_addr)
                          recover_list);
  if not (Queue.is_empty recover_list) then disable_gossip ()



