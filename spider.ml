(***********************************************************************)
(* spider.ml - start with a SKS server and spider the entire network   *)
(*             by recursively crawling peers from stats pages          *)
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
open Pstyle
open Common
module Set = PSet.Set
module Unix = UnixLabels

let stats_timeout = 10

(** Argument parsing *)
let root =
  if Array.length Sys.argv = 2 then
    (Sys.argv.(1),11370)
  else
    ("pool.sks-keyservers.net",11370)

let input_lines cin =
  let rec loop lines =
    match (try Some (input_line cin)
           with End_of_file -> None)
    with
      None -> List.rev lines
    | Some l -> loop (l::lines)
  in
  loop []

let get_ip_opt hostname =
  if hostname = "localhost" then None
  else
    try
      let he = Unix.gethostbyname hostname in
      Some he.Unix.h_addr_list
    with
      Invalid_argument _ | Not_found -> None

let fetch_url url =
  let cin = Unix.open_process_in (sprintf "curl -s -m %d \"%s\"" stats_timeout url) in
  let lines = input_lines cin in
  match Unix.close_process_in cin with
  | Unix.WEXITED 0 -> Some lines
  | _ -> None

let start_line = Str.regexp "<h2>Gossip Peers.*"
let whitespace = Str.regexp "[ \t<]+"
let end_td = Str.regexp "</td></tr>$"

let get_peer line =
  if line </> (0,8) = "<tr><td>" then
    match Str.split whitespace (Str.global_replace end_td "" line </> (8,0)) with
    | host::port::_ ->
        let port = int_of_string port in
        Some (host,port)
    | _ -> None
  else
    None

let build_url (host,port) =
  sprintf "http://%s:%d/pks/lookup?op=stats" host port

let lines_to_peers lines =
  let rec skip_to_start = function
    | line::((_::rest) as tl) ->
        if Str.string_match start_line line 0 then
          rest
        else skip_to_start tl
    | _ -> []
  in
  let lines = skip_to_start lines in
  let rec get_peers = function
    | [] -> []
    | hd::tl -> match get_peer hd with
      | Some peer -> peer :: get_peers tl
      | None -> []
  in
  get_peers lines

let multi_fetch (host,port) =
  let ports = [port+1] in
  (*let ports = if port <> 11370 then 11371::ports else ports in
  let ports = List.rev (80::ports) in *)
  let get_peers (host,port) =
    match fetch_url (build_url (host,port)) with
    | None -> None
    | Some x ->
        let peers = lines_to_peers x in
        if peers = [] then None
        else Some peers
  in
  let rec loop ports = match ports with
      [] -> None
    | port::tl ->
        match get_peers (host,port) with
        | Some x -> Some x
        | None -> loop tl
  in
  loop ports

let find_all peer =
  let visited = ref (Set.singleton None) in
  let rec dfs peer =
    let ip = get_ip_opt (fst peer) in
    if Set.mem ip !visited then
      []
    else
      begin
        visited := Set.add ip !visited;
        match multi_fetch peer with
        | None -> (* retrieval failed *)
            eprintf "(%s,%d) FAILED\n%!" (fst peer) (snd peer);
            []
        | Some peers ->
            try
              eprintf "(%s,%d)\n%!" (fst peer) (snd peer);
              let others = List.concat (List.map ~f:dfs peers) in
              peer :: others
            with e ->
              eprintf "(%s,%d) FAILED with %s\n%!" (fst peer) (snd peer)
                (Printexc.to_string e);
              []
      end
  in
  dfs peer


let () = if not !Sys.interactive then
  let servers = find_all root in
  printf "%d servers found\n" (List.length servers);
  List.iter ~f:(fun (host,port) -> printf "%s %d\n" host port) servers
