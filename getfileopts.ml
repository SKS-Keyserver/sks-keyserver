(***********************************************************************)
(* getfileopts.ml - Loads settings from settings file.                 *)
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
open Settings
open Pstyle

let protect ~f ~(finally: unit -> unit) =
  let result = ref None in
  try
    result := Some (f ());
    raise Exit
  with
      Exit as e ->
        finally ();
        (match !result with Some x -> x | None -> raise e)
    | e ->
        finally (); raise e

let whitespace c = c = '\t' || c = ' ' || c = '\n'

let strip s =
  let lower = ref 0 in
  while !lower < String.length s && whitespace s.[!lower] do
    incr lower
  done;

  let upper = ref (String.length s - 1) in
  while !upper >= 0 && whitespace s.[!upper] do
    decr upper
  done;

  if !upper < !lower then ""
  else
    String.sub s ~pos:!lower ~len:(!upper - !lower + 1)

let csplit c s =
  let i = String.index s c in
  (strip (String.sub ~pos:0 ~len:i s),
   strip (String.sub ~pos:(i+1) ~len:(String.length s - i - 1) s)
  )

let decomment l =
  let l =
    try
      let pos = String.index l '#' in
      String.sub l ~pos:0 ~len:pos
    with
        Not_found -> l
  in
  strip l

(** convert a line of the config line to command-line format *)
let line_convert l =
  let l = decomment l in
  if String.length l = 0 then None
  else
    let (command,arg) = csplit ':' l in
    Some [ "-" ^ command ; arg ]

(** read in file and convert it to command-line format *)
let file_convert f =
  let rec loop accum =
    match (try Some (input_line f) with End_of_file -> None)
    with
      | Some l -> (
          match line_convert l with
              None -> loop accum
            | Some l -> loop (l :: accum)
        )
      | None -> "" :: List.concat (List.rev accum)
  in
  Array.of_list (loop [])

let fname_convert fname =
  if Sys.file_exists fname then
    try
      let f = open_in fname in
      protect ~f:(fun () -> file_convert f)
        ~finally:(fun () -> close_in f)
    with
        Sys_error _ as e -> failwith
          (sprintf "Sys error while parsing config file: %s"
             (Printexc.to_string e) )
  else
    [||]

(**************************************************************)
(**************************************************************)
(**************************************************************)

let config_fname = "sksconf"

let parse args =
  Arg.current := 0;
  Arg.parse_argv args parse_spec anon_options usage_string

let () =

  try
    let pos = ref 0 in
    while !pos < Array.length Sys.argv && Sys.argv.(!pos) <>
      "-read_config_file"
    do incr pos done;

    if !pos = Array.length Sys.argv
    then (
      parse Sys.argv;
      let from_file_commandline =
        fname_convert (Filename.concat !basedir config_fname)
      in
      parse from_file_commandline
    )
    else (
      parse (Sys.argv <|> (0,!pos));
      let from_file_commandline =
        fname_convert (Filename.concat !basedir config_fname)
      in
      parse from_file_commandline;
      parse (Array.append [|""|] (Sys.argv <|> (!pos + 1,0)))
    );

    anonlist := List.rev !anonlist;
    anonlist := List.filter ~f:(( <> ) "") !anonlist
  with
    | Arg.Bad s ->
        print_string s;
        exit (-1)
    | Arg.Help s ->
        print_string s;
        exit 0

