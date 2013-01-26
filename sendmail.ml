(***********************************************************************)
(* sendmail.ml - Simple (& likely incomplete) interface for sending    *)
(*               mail                                                  *)
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
open Printf

module Map = PMap.Map
module Set = PSet.Set

type msg = { headers: (string * string) list;
             body: string;
           }

let process_status_to_string ps =
  let (name,code) = match ps with
      Unix.WEXITED n -> ("Exited",n)
    | Unix.WSIGNALED n -> ("Signaled",n)
    | Unix.WSTOPPED n -> ("Stopped",n)
  in
  sprintf "%s(%d)" name code

exception Unwrap_failure
let unwrap x = match x with
    None -> raise Unwrap_failure
  | Some x -> x


(** Invokes sendmail and sends the argument to sendmail via stdin *)
let send_text text =
  let cout = Unix.open_process_out !Settings.sendmail_cmd in
  let status = ref None in
  protect ~f:(fun () -> output_string cout text)
    ~finally:(fun () -> status := Some (Unix.close_process_out cout));
  if unwrap !status <> Unix.WEXITED 0 then
    failwith (sprintf "Sendmail.send_text failed: %s"
                (process_status_to_string (unwrap !status)))
  else ()

(** converts message to string ready for sending via you favoriate
  MTA *)
let msg_to_string msg =
  let header_lines =
    List.map ~f:(fun (field,entry) ->
                   if field = "" then sprintf "\t%s\n" entry
                   else sprintf "%s: %s\n" field entry)
      msg.headers
  in
  let header = String.concat ~sep:"" header_lines in
  header ^ "\n" ^ msg.body


(** Sends the given message *)
let send msg = send_text (msg_to_string msg)

(** removes the continuation of the headers, where a continuation is defined
  to be an initial sequence of headers with empty field names
*)
let rec remove_continuation headers =  match headers with
    [] -> []
  | ("",entry)::tl ->
      remove_continuation tl
  | headers -> headers


let rec filter_headers_from_headers headers fields = match headers with
  | [] -> []
  | (("",contents) as hd)::tl ->
      hd::(filter_headers_from_headers tl fields)
  | ((field,contents) as hd)::tl ->
      if Set.mem (String.lowercase field) fields then
        hd::(filter_headers_from_headers tl fields)
      else
        filter_headers_from_headers (remove_continuation tl)
          fields

let filter_headers msg fields =
  let fields = Set.of_list (List.map ~f:String.lowercase fields) in
  { msg with
      headers = filter_headers_from_headers msg.headers fields
  }

let add_headers msg headers =
  { msg with headers = headers @ msg.headers }

let get_body msg = msg.body
let get_headers msg = msg.headers
