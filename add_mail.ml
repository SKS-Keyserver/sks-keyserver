(***********************************************************************)
(* add_mail.ml - Executable: interprets stdin as mail message and      *)
(*               posts content to specified HTTP address               *)
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
module Unix = UnixLabels
module Map = PMap.Map
module Set = PSet.Set

(** Argument parsing *)

let anonymous = ref []

let usage_string =
  Sys.argv.(0) ^ " sks_directory_name"

let anon_options option =
  anonymous := option::!anonymous

let parse_spec = [ ]

let dirname =
  Arg.parse parse_spec anon_options usage_string;
  if List.length !anonymous <> 1
  then (
    printf "Wrong number (%d) of arguments given.  %s\n"
          (List.length !anonymous)
          usage_string;
    exit (-1)
  ) else
    Filename.concat (List.hd !anonymous) "messages"

(** dumps contents of one file into another *)
let pipe_file =
  let blocksize = 100 * 1024 in
  let buf = String.create blocksize in
  let rec pipe_file file1 file2 =
    let bytes_read = input file1 buf 0 blocksize in
    if bytes_read <> 0 then (
      output file2 buf 0 bytes_read;
      pipe_file file1 file2
    )
  in
  pipe_file

let run () =
  if not (Sys.file_exists dirname)
  then Unix.mkdir dirname 0o700;
  let fname = sprintf "msg-%08d" (Random.int 100000000) in
  let fname = Filename.concat dirname fname in
  let f = open_out fname in
  pipe_file stdin f;
  close_out f;
  Sys.rename fname (fname ^ ".ready")

let () =
  Random.self_init ();
  run ()
