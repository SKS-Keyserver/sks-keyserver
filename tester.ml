(***********************************************************************)
(* tester.ml                                                           *)
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
open DbMessages
module Unix = UnixLabels

let settings = {
  Keydb.withtxn = !Settings.transactions;
  Keydb.cache_bytes = !Settings.cache_bytes;
  Keydb.pagesize = !Settings.pagesize;
  Keydb.keyid_pagesize = !Settings.keyid_pagesize;
  Keydb.meta_pagesize = !Settings.meta_pagesize;
  Keydb.subkeyid_pagesize = !Settings.subkeyid_pagesize;
  Keydb.time_pagesize = !Settings.time_pagesize;
  Keydb.tqueue_pagesize = !Settings.tqueue_pagesize;
  Keydb.word_pagesize = !Settings.word_pagesize;
  Keydb.dbdir = Lazy.force Settings.dbdir;
  Keydb.dumpdir = Lazy.force Settings.dumpdir;
}

module Keydb = Keydb.Safe


let send_msg addr msg =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0 in
  protect ~f:( fun () ->
                 Unix.connect s ~addr:addr;
                 let cin = Channel.sys_in_from_fd s
                 and cout = Channel.sys_out_from_fd s in
                 marshal cout msg;
                 let reply = unmarshal cin in
                 printf "Reply received: %s\n" (msg_to_string reply.msg);
                 reply
             )
    ~finally:(fun () -> Unix.close s)

let send_msg_noreply addr msg =
  let s = Unix.socket
            ~domain:(Unix.domain_of_sockaddr addr)
            ~kind:Unix.SOCK_STREAM
            ~protocol:0 in
  protect ~f:(fun () ->
                Unix.connect s ~addr:addr;
                let cout = Channel.sys_out_from_fd s in
                marshal cout msg
             )
    ~finally:(fun () -> Unix.close s)


let print_key key =
  let ids = Key.get_ids key in
  List.iter ~f:(printf "%s | ") ids;
  print_newline ()

let word_query addr string =
  let words = Utils.extract_words string in
  let reply = send_msg addr (WordQuery words) in
  match reply.msg with
    | Keys keys ->
        List.iter ~f:print_key keys;
        printf "\n-------------------\n"
    | _ ->
        printf "Unexpected response\n"; flush stdout

let rec is_sorted list = match list with
    [] -> true
  | hd::[] -> true
  | hd1::hd2::tl -> hd2 > hd1 && is_sorted (hd2::tl)

let rec last list = match list with
    [] -> raise Not_found
  | hd::[] -> hd
  | hd::tl -> last tl

let get_log addr ts =
  let resp = send_msg addr (LogQuery ts) in
  match resp.msg with
      LogResp log -> log
    | _ -> failwith "Unexpected response"

let ts pair = fst pair

let first log = List.hd log
let first_ts log = ts (first log)

let last_ts log =
  let (ts,hash) = last log in
  ts

(*
let rec get_all ts accum =
  let hashes = send_msg (LogQuery ts)

*)
