(***********************************************************************)
(* reconMessages.ml                                                    *)
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
open CMarshal
open Common
module Unix=UnixLabels
module Map = PMap.Map

(***********************************)
(* ZZ-specific marshallers ********)
(***********************************)

let marshal_ZZp cout zz =
  let str = ZZp.to_bytes zz in
  marshal_lstring cout str

let unmarshal_ZZp cin =
  ZZp.of_bytes (unmarshal_lstring !ZZp.nbytes cin)

(*****)

let marshal_zzarray cout zzarray =
  marshal_array ~f:marshal_ZZp cout
    (ZZp.mut_array_to_array zzarray)

let unmarshal_zzarray cin =
  let array = unmarshal_array ~f:unmarshal_ZZp cin in
  ZZp.mut_array_of_array array

(*****)

let marshal_zset cout set =
  let array = Array.of_list (ZZp.Set.elements set) in
  marshal_array ~f:marshal_ZZp cout array


let unmarshal_zset cin =
  let array = unmarshal_array ~f:unmarshal_ZZp cin in
  ZZp.zset_of_list (Array.to_list array)

(***********************************)
(* Data Types  ********************)
(***********************************)

(* recon request where polynomial checksum is sent *)
type recon_rqst_poly =
    { rp_prefix: Bitstring.t;
      rp_size: int;
      rp_samples: ZZp.mut_array;
    }


let marshal_recon_rqst_poly cout rp =
  marshal_bitstring cout rp.rp_prefix;
  cout#write_int rp.rp_size;
  marshal_zzarray cout rp.rp_samples

let unmarshal_recon_rqst_poly cin =
  let prefix = unmarshal_bitstring cin in
  let size = cin#read_int in
  let samples = unmarshal_zzarray cin in
  { rp_prefix = prefix;
    rp_size = size;
    rp_samples = samples;
  }

(***********************************)
(***********************************)
(***********************************)

(* recon request where full data is sent *)
type recon_rqst_full =
    { rf_prefix: Bitstring.t;
      rf_elements: ZZp.Set.t;
    }

let marshal_recon_rqst_full cout rf =
  marshal_bitstring cout rf.rf_prefix;
  marshal_zset cout rf.rf_elements

let unmarshal_recon_rqst_full cin =
  let prefix = unmarshal_bitstring cin in
  let elements = unmarshal_zset cin in
  { rf_prefix = prefix;
    rf_elements = elements; }

(***********************************)
(***********************************)
(***********************************)

(* recon request where full data is sent *)
type configdata = (string,string) Map.t
(* type metadata = { md_recon_addr: Unix.sockaddr; } *)

let marshal_stringpair cout (s1,s2) =
  marshal_string cout s1; marshal_string cout s2

let unmarshal_stringpair cin =
  let s1 = unmarshal_string cin in
  let s2 = unmarshal_string cin in
  (s1,s2)

let marshal_stringpair_list cout list =
  marshal_list ~f:marshal_stringpair cout list

let unmarshal_stringpair_list cin =
  unmarshal_list ~f:unmarshal_stringpair cin

let marshal_configdata cout configdata =
  marshal_stringpair_list cout (Map.to_alist configdata)

let unmarshal_configdata cin =
  Map.of_alist (unmarshal_stringpair_list cin)

let sockaddr_to_string sockaddr = match sockaddr with
    Unix.ADDR_UNIX s -> sprintf "<ADDR_UNIX %s>" s
  | Unix.ADDR_INET (addr,p) -> sprintf "<ADDR_INET [%s]:%d>"
      (Unix.string_of_inet_addr addr) p


(***********************************)
(***********************************)
(***********************************)


let marshal_allreply cout (prefix,set) =
  marshal_bitstring cout prefix;
  marshal_zset cout set

let unmarshal_allreply cin =
  let prefix = unmarshal_bitstring cin in
  let set = unmarshal_zset cin in
  (prefix,set)

(*************)

type msg = | ReconRqst_Poly of recon_rqst_poly
           | ReconRqst_Full of recon_rqst_full
           | Elements of ZZp.Set.t
           | FullElements of ZZp.Set.t
           | SyncFail
           | Done
           | Flush
           | Error of string
           | DbRqst of string
           | DbRepl of string
           | Config of configdata

let rec msg_to_string msg =
  (match msg with
     | ReconRqst_Poly rp ->
         sprintf "ReconRqst_Poly(%s)" (Bitstring.to_string rp.rp_prefix)
     | ReconRqst_Full rf ->
         sprintf "ReconRqst_Full(%d,%s)"
         (ZZp.Set.cardinal rf.rf_elements)
         (Bitstring.to_string rf.rf_prefix)
     | Elements s -> sprintf "Elements(len:%d)" (ZZp.Set.cardinal s)
     | FullElements s -> sprintf "FullElements(len:%d)" (ZZp.Set.cardinal s)
     | SyncFail -> "SyncFail"
     | Done -> "Done"
     | Flush -> "Flush"
     | Error s -> sprintf "Error(%s)" s
     | DbRqst s -> "DbRqst"
     | DbRepl s -> "DbRepl"
     | Config s -> "Config"
  )

let print_msg msg = print_string (msg_to_string msg)

let marshal_samplevalues cout (size,sarray) =
  cout#write_int size;
  marshal_fixed_sarray cout sarray

let unmarshal_samplevalues cin =
  let size = cin#read_int in
  let sarray = unmarshal_fixed_sarray cin in
    (size,sarray)

let marshal_time = ref 0.0
let unmarshal_time = ref 0.0
let timer = MTimer.create ()

let rec marshal_msg cout msg = match msg with
  | ReconRqst_Poly rp -> cout#write_byte 0; marshal_recon_rqst_poly cout rp
  | ReconRqst_Full rf -> cout#write_byte 1; marshal_recon_rqst_full cout rf
  | Elements set ->      cout#write_byte 2; marshal_zset cout set
  | FullElements set ->  cout#write_byte 3; marshal_zset cout set
  | SyncFail ->          cout#write_byte 4
  | Done ->              cout#write_byte 5;
  | Flush ->             cout#write_byte 6;
  | Error s ->           cout#write_byte 7; marshal_string cout s
  | DbRqst s ->             cout#write_byte 8; marshal_string cout s
  | DbRepl s ->             cout#write_byte 9; marshal_string cout s
  | Config md ->       cout#write_byte 10; marshal_configdata cout md


let rec unmarshal_msg cin =
  let msg_type = cin#read_byte in
  match msg_type with
    | 0 -> ReconRqst_Poly (unmarshal_recon_rqst_poly cin)
    | 1 -> ReconRqst_Full (unmarshal_recon_rqst_full cin)
    | 2 -> Elements (unmarshal_zset cin)
    | 3 -> FullElements (unmarshal_zset cin)
    | 4 -> SyncFail
    | 5 -> Done
    | 6 -> Flush
    | 7 -> Error (unmarshal_string cin)
    | 8 -> DbRqst (unmarshal_string cin)
    | 9 -> DbRepl (unmarshal_string cin)
    | 10 -> Config (unmarshal_configdata cin)
    | x -> failwith (sprintf "Unexpected message code: %d" x)

module M =
  NbMsgContainer.Container(
    struct
      type msg_t = msg
      let marshal = marshal_msg
      let unmarshal = unmarshal_msg
      let to_string = msg_to_string
      let print = (fun s -> plerror 6 "%s" s)
    end)

include M



(* type init_flag = Recon | DbRequest

let init_flag_to_byte flag = match flag with
    Recon -> 0
  | DbRequest -> 1

let init_flag_of_byte byte = match byte with
    0 -> Recon
  | 1 -> DbRequest
  | _ -> failwith "Unexpected DB flag"
*)


