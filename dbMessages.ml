(***********************************************************************)
(* dbMessages.ml- Message types for communicating with com ports on    *)
(*                dbserver and reconserver                             *)
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

open MoreLabels
open StdLabels
open Packet
open CMarshal
open Common
open Printf
module Unix=UnixLabels
module Set = PSet.Set


(***********************************)

type configvar = [ `int of int | `float of float | `string of string | `none ]

let marshal_config cout (s,cvar) =
  marshal_string cout s;
  match cvar with
    | `int x -> cout#write_byte 0; cout#write_int x
    | `float x -> cout#write_byte 1; cout#write_float x
    | `string x -> cout#write_byte 2; marshal_string cout x
    | `none -> cout #write_byte 3

let unmarshal_config cin =
  let s = unmarshal_string cin in
  let cvar =
    match cin#read_byte with
      | 0 -> `int cin#read_int
      | 1 -> `float cin#read_float
      | 2 -> `string (unmarshal_string cin)
      | 3 -> `none
      | _ -> failwith "Type failure unmarshalling config variable"
  in
  (s,cvar)

(***********************************)
(* Data Types  ********************)
(***********************************)

type msg = | WordQuery of string list
           | LogQuery of (int * timestamp) (* must make other changes.... *)
           | HashRequest of string list
           | LogResp of ( timestamp * event) list
           | Keys of key list
           | KeyStrings of string list
           | Ack of int
           | MissingKeys of (string list * Unix.sockaddr) (* DEPRECATED *)
           | Synchronize
           | RandomDrop of int
           | ProtocolError
           | DeleteKey of string
           | Config of (string * configvar)
           | Filters of string list

(****  data specific marshallers  ****)

let marshal_timestamp cout timestamp = cout#write_float timestamp
let unmarshal_timestamp cin = cin#read_float

let marshal_logquery cout logquery =
  let (count,timestamp) = logquery in
  cout#write_int count;
  marshal_timestamp cout timestamp

let unmarshal_logquery cin =
  let count = cin#read_int in
  let timestamp = unmarshal_timestamp cin in
  (count,timestamp)

let marshal_event cout event =  match event with
  | Add hash -> cout#write_byte 0; marshal_string cout hash
  | Delete hash -> cout#write_byte 1; marshal_string cout hash

let unmarshal_event cin =
  match cin#read_byte with
      0 -> Add (unmarshal_string cin)
    | 1 -> Delete (unmarshal_string cin)
    | _ -> failwith "Unexpected code for event"

let marshal_log_entry cout ( timestamp , event ) =
  marshal_timestamp cout timestamp;
  marshal_event cout event

let unmarshal_log_entry cin =
  let timestamp = unmarshal_timestamp cin in
  let event = unmarshal_event cin in
  (timestamp,event)

let marshal_key cout key = marshal_string cout (Key.to_string key)
let unmarshal_key cin = Key.of_string (unmarshal_string cin)

let marshal_key_list l = marshal_list ~f:marshal_key l
let unmarshal_key_list l = unmarshal_list ~f:unmarshal_key l

let marshal_missingkeys cout (list,sockaddr) =
  marshal_list ~f:marshal_string cout list;
  marshal_sockaddr cout sockaddr

let unmarshal_missingkeys cin =
  let list = unmarshal_list ~f:unmarshal_string cin in
  let sockaddr = unmarshal_sockaddr cin in
  (list,sockaddr)

(********************************************************)

let marshal_msg cout msg =
  match msg with
     | WordQuery x -> cout#write_byte 0; marshal_list ~f:marshal_string cout x
     | LogQuery x -> cout#write_byte 1; marshal_logquery cout x
     | LogResp x -> cout#write_byte 2; marshal_list ~f:marshal_log_entry cout x
     | Keys x -> cout#write_byte 3; marshal_list ~f:marshal_key cout x
         (* keystrings is just an alias for keys. They're sent over the wire
            in the same form *)
     | KeyStrings x -> cout#write_byte 3; marshal_list ~f:marshal_string cout x
     | Ack x -> cout#write_byte 4; cout#write_int x
     | MissingKeys x -> failwith "DO NOT USE MissingKeys"
         (* cout#write_byte 5; marshal_missingkeys cout x*)
     | Synchronize -> cout#write_byte 6
     | RandomDrop x -> cout#write_byte 7; cout#write_int x
     | ProtocolError -> cout#write_byte 8
     | DeleteKey s -> cout#write_byte 9; marshal_string cout s
     | HashRequest x -> cout#write_byte 10; marshal_list ~f:marshal_string cout x
     | Config x ->            cout#write_byte 11; marshal_config cout x
     | Filters x -> cout#write_byte 12; marshal_list ~f:marshal_string cout x


let rec unmarshal_msg cin =
  let rval =
  match cin#read_byte with
    | 0 -> WordQuery (unmarshal_list ~f:unmarshal_string cin)
    | 1 -> LogQuery (unmarshal_logquery cin)
    | 2 ->
        LogResp (unmarshal_list ~f:unmarshal_log_entry cin)
    | 3 -> Keys (unmarshal_list ~f:unmarshal_key cin)
    | 4 -> Ack cin#read_int
    | 5 -> MissingKeys (unmarshal_missingkeys cin)
    | 6 -> Synchronize
    | 7 -> RandomDrop cin#read_int
    | 8 -> ProtocolError
    | 9 -> DeleteKey (unmarshal_string cin)
    | 10 -> HashRequest (unmarshal_list ~f:unmarshal_string cin)
    | 11 -> Config (unmarshal_config cin)
    | 12 -> Filters (unmarshal_list ~f:unmarshal_string cin)
    | _ -> failwith "Unexpected message type"
  in
  rval

let sockaddr_to_string sockaddr = match sockaddr with
    Unix.ADDR_UNIX s -> sprintf "<ADDR_UNIX %s>" s
  | Unix.ADDR_INET (addr,p) -> sprintf "<ADDR_INET [%s]:%d>" (Unix.string_of_inet_addr addr) p

let msg_to_string msg =
  match msg with
      WordQuery words -> "WordQuery: " ^ (String.concat ", " words)
    | LogQuery (count,timestamp) -> sprintf "LogQuery: (%d,%f)" count timestamp
    | LogResp list ->
        let length = List.length list in
        sprintf "LogResp: %d events" length
    | Keys keys ->
        let length = List.length keys in
        sprintf "Keys: %d keys" length
    | KeyStrings keystrings ->
        let length = List.length keystrings in
        sprintf "KeyStrings: %d keystrings" length
    | Ack i ->
        sprintf "Ack: %d" i
    | MissingKeys (keys,sockaddr) ->
        if List.length keys > 20 then
          sprintf "MissingKeys: %d keys from %s"
            (List.length keys) (sockaddr_to_string sockaddr)
        else
          sprintf "MissingKeys from %s: [ %s ]"
            (sockaddr_to_string sockaddr)
            (String.concat ~sep:""
               (List.map ~f:(sprintf "\n\t%s")
                  (List.map Utils.hexstring keys)))
    | Synchronize -> sprintf "Synchronize"
    | RandomDrop i ->
        sprintf "RandomDrop: %d" i
    | ProtocolError -> "ProtocolError"
    | DeleteKey x -> sprintf "DeleteKey %s" (Utils.hexstring x)
    | HashRequest x -> sprintf "HashRequest(%d)" (List.length x)
    | Config (s,cvar) -> sprintf "Config(s," ^
         (match cvar with
              `int x -> sprintf "%d)" x
            | `float x -> sprintf "%f)" x
            | `string x -> sprintf "%s)" x
            | `none -> "none)"
         )
    | Filters filters -> sprintf "Filters(%s)"
        (String.concat ~sep:"," filters)


module M =
  MsgContainer.Container(
    struct
      type msg_t = msg
      let marshal = marshal_msg
      let unmarshal = unmarshal_msg
      let to_string = msg_to_string
      let print = (fun s -> plerror 7 "%s" s)
    end
  )

include M
