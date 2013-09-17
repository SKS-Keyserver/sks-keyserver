(***********************************************************************)
(* request.ml                                                          *)
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

let amp = Str.regexp "&"

let chsplit c s =
  let eqpos = String.index s c in
  let first = Str.string_before s eqpos
  and second = Str.string_after s (eqpos + 1) in
  (first, second)

let eqsplit s = chsplit '=' s

type request_kind = VIndex | Index | Get | HGet | Stats

type request = { kind: request_kind;
                 search: string list;
                 fingerprint: bool;
                 hash: bool;
                 exact: bool;
                 machine_readable: bool;
                 clean: bool;
                 limit: int;
               }

let default_request = { kind = Index;
                        search = [];
                        fingerprint = false;
                        hash = false;
                        exact = false;
                        machine_readable = false;
                        clean = true;
                        limit = (-1);
                      }

let comma_rxp = Str.regexp ","

let rec request_of_oplist ?(request=default_request) oplist =
  match oplist with
      [] -> request
    | hd::tl ->
        let new_request =
          match hd with
            | ("options",options) ->
                let options = Str.split comma_rxp options in
                if List.mem "mr" options
                then { request with machine_readable = true }
                else request
            | ("op","stats") -> {request with kind = Stats };
            | ("op","x-stats") -> {request with kind = Stats };
            | ("op","index") -> {request with kind = Index };
            | ("op","vindex") -> {request with kind = VIndex };
            | ("op","get") -> {request with kind = Get};
            | ("op","hget") -> {request with kind = HGet};
            | ("op","x-hget") -> {request with kind = HGet};
            | ("limit",c) -> {request with limit = (int_of_string c)};
            | ("search",s) ->
                {request with search =
                   List.rev (Utils.extract_words (String.lowercase s))
                };
            | ("fingerprint","on") ->  {request with fingerprint = true};
            | ("fingerprint","off") ->  {request with fingerprint = false};
            | ("hash","on") ->  {request with hash = true};
            | ("hash","off") ->  {request with hash = false};
            | ("x-hash","on") ->  {request with hash = true};
            | ("x-hash","off") ->  {request with hash = false};
            | ("exact","on") ->  {request with exact = true};
            | ("exact","off") ->  {request with exact = false};
            | ("clean","on") -> {request with clean = true;}
            | ("clean","off") -> {request with clean = false;}
            | ("x-clean","on") -> {request with clean = true;}
            | ("x-clean","off") -> {request with clean = false;}
            | _ -> request
        in
        request_of_oplist tl ~request:new_request
