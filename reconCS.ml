(***********************************************************************)
(* reconCS.ml - Reconciliation logic that is shared between the client *)
(*              and server                                             *)
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
open CMarshal
open ReconMessages
open Printf
module Set = PSet.Set
module Map = PMap.Map
module Unix = UnixLabels

(** Configuration related functions *)

(** Build map containing configuration information *)
let build_configdata filters =
  let map = Map.empty in
  let map = (map |< "version") version in
  let map = (map |< "http port") (int_to_string http_port) in
  let map = (map |< "bitquantum") (int_to_string !Settings.bitquantum) in
  let map = (map |< "mbar") (int_to_string !Settings.mbar) in
  let map = (map |< "filters") (String.concat ~sep:"," filters) in
  map

let comma_rxp = Str.regexp ","
let config_get_filters cd = Str.split comma_rxp (cd |= "filters")

(** Returns `passed if there are no problems with the configdata,
  `failed s if there is a problem, where s is a string describing the
  problem.
*)
let test_configdata local remote =
  try
    let remote_version_string =  remote |= "version" in
    let remote_version = parse_version_string remote_version_string in
    if remote_version < compatible_version_tuple
    then `failed (sprintf "Requires version at least %s.  %s provided "
                    compatible_version_string remote_version_string)
    else if not (Set.equal
                   (Set.of_list (config_get_filters local))
                   (Set.of_list (config_get_filters remote)))
    then `failed (sprintf "filters do not match.\n\tlocal filters: %s\n\tremote filters: %s"
                    (MList.to_string  ~f:(sprintf "%s")
                       (config_get_filters local))
                    (MList.to_string ~f:(sprintf "%s")
                       (config_get_filters remote))
                 )
    else
      let bitquantum = int_of_string (remote |= "bitquantum") in
      let mbar = int_of_string (remote |= "mbar") in
      if bitquantum <> !Settings.bitquantum then
        `failed "bitquantum values do not match"
      else if mbar <> !Settings.mbar then
        `failed "mbar values do not match"
      else
        `passed
  with
      Not_found -> `failed "Missing entry in configdata"
    | e ->
        Eventloop.reraise e;
        `failed (sprintf "Error parsing configdata: %s"
                   (Printexc.to_string e) )

(** Exchanges config data with other host, and tests
  whether provided config data allows for reconciliation
  to proceed.

  @param cin input channel @param cout output channel
  @param filters list of strings representing filters that have been applied
  to data.
  @param peer sockaddr of gossip partner
*)
let handle_config cin cout filters peer =
  let configdata = build_configdata filters in
  marshal cout (Config configdata); (* channel is flushed here *)
  let remote_configdata =
    match (unmarshal cin).msg with
      | Config x -> x
      | _ -> failwith "No configdata provided"
  in
  (match test_configdata configdata remote_configdata with
     | `passed ->
         marshal_string cout "passed";
         cout#flush
     | `failed reason ->
         marshal_string cout "failed";
         marshal_string cout reason;
         cout#flush;
        failwith (sprintf "configuration of remote host (%s) rejected: %s"
                    (sockaddr_to_string peer) reason)
  );
  (match unmarshal_string cin with
       "passed" -> ()
     | "failed" ->
         let reason = unmarshal_string cin in
         failwith (sprintf "Local configuration rejected by remote host (%s): %s"
                     (sockaddr_to_string peer) reason)
     | _ -> failwith "Unexpected configuration confirmation response"
  );
  remote_configdata


let config_get_http_port cd =
  int_of_string (cd |= "http port")

let change_port sockaddr newport = match sockaddr with
  | Unix.ADDR_UNIX _ -> raise (Invalid_argument
                                 "Can't change port of UNIX address")
  | Unix.ADDR_INET (ipaddr,port) -> Unix.ADDR_INET (ipaddr,newport)

let print_config config =
  perror "Printing config";
  Map.iter ~f:(fun ~key ~data -> perror "   %s: %s" key data) config



(** function to connect to remote host to initate reconciliation *)
let connect tree ~filters ~partner =
  (* TODO: change the following to depend on the address type *)
  let s = Unix.socket partner.Unix.ai_family partner.Unix.ai_socktype partner.Unix.ai_protocol
  in
  let run () =
    Unix.bind s (match_client_recon_addr partner.Unix.ai_addr);
    Unix.connect s partner.Unix.ai_addr;
    let cin = Channel.sys_in_from_fd s
    and cout = Channel.sys_out_from_fd s in
    plerror 4 "Initiating reconciliation";
    let remote_config = handle_config cin cout filters partner.Unix.ai_addr in
    ignore (Unix.alarm !Settings.reconciliation_timeout);

    let http_port = config_get_http_port remote_config in
    let remote_http_address = change_port partner.Unix.ai_addr http_port in

    let data = Server.handle tree cin cout in
    (data,remote_http_address)
  in
  protect ~f:run ~finally:(fun () -> Unix.close s)


(** *)
let handle_connection tree ~filters ~partner cin cout  =

  plerror 4 "Joining reconciliation";
  let remote_config = handle_config cin cout filters partner in
  ignore (Unix.alarm !Settings.reconciliation_timeout);

  let http_port = config_get_http_port remote_config in
  let remote_http_address = change_port partner http_port in

  let data = Client.handle tree cin cout in

  (data,remote_http_address)
