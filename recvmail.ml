(***********************************************************************)
(* recvmail.ml - Simple (and likely incomplete) interface for          *)
(*               receiving mail                                        *)
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

open Common
open StdLabels
open MoreLabels
open Printf
module Unix = UnixLabels

let whitespace = Str.regexp "[ \t\n\r]+"
let eol = Str.regexp "\r?\n"

let parse_header_line hline =
  if String.length hline = 0
  then None (* done parsing header *)
  else
    if hline.[0] = '\t'
    then (* this is a continuation, not a new pair *)
      Some ("",String.sub ~pos:1 ~len:(String.length  hline - 1) hline)
    else

      try
        let colonpos =
          try String.index hline ':'
          with Not_found -> failwith "No colon found"
        in
        let key = String.sub hline ~pos:0 ~len:colonpos
        and data =  String.sub hline ~pos:(colonpos+1)
                      ~len:(String.length hline - colonpos - 1)
        in
        if String.contains data ' ' then
          (* then the colon in question wasn't a real line *)
          Some ("",Wserver.strip hline)
        else
          Some (Wserver.strip key, Wserver.strip data)

      with
          Failure "No colon found" -> Some ("",Wserver.strip hline)



let rec parse_header lines header = match lines with
    [] ->
      (* headers done, no body left *)
      (List.rev header,[])
  | hline::tl -> match parse_header_line hline with
        None -> (List.rev header,tl)
      | Some pair -> parse_header tl (pair::header)


(** Given a list of headers where some entries have no keys listed, returns a
  list of headers where those keyless entries have been joined into previous
  entries.
*)
let rec simplify_headers headers newheaders =
  match headers with
      [] -> List.rev newheaders
    | ("",data)::header_tl ->
      (match newheaders with
           [] -> failwith "simplify_headers: initial header line lacks field"
         | (key,prevdata)::newheader_tl ->
             simplify_headers
             header_tl ((key,prevdata ^ "\n" ^ data)::newheader_tl)
      )
    | (key,data)::header_tl ->
        simplify_headers header_tl ((key,data)::newheaders)

let simplify_headers headers = simplify_headers headers []

let parse msgtext =
  let lines = Str.split eol msgtext in
  let (headers,bodylines) = parse_header lines [] in
  (*let headers = simplify_headers headers in *)
  { Sendmail.headers = headers;
    Sendmail.body = String.concat ~sep:"\n" bodylines;
  }

