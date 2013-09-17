(***********************************************************************)
(* version.ml - Executable: Show version information                   *)
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

open Printf

let run () =
  let bdb_version = Bdb.version () in
  let dbstats_dir =
    let split = Str.regexp_string "." in
    let major_minor_string major minor =
      sprintf "Further details about the BDB environment can be seen by \
	  executing\ndb%s.%s_stat -x in the KDB and Ptree directories\n" major minor
    in
    match Str.split split bdb_version with
    | major :: minor :: _ -> major_minor_string major minor
    | [] | _ :: []        -> major_minor_string "X"   "Y"
  in
  printf "SKS version %s%s\n"
    Common.version Common.version_suffix;
	
  printf "Compiled with Ocaml version %s and BDB version %s\n"
      Sys.ocaml_version bdb_version;

  printf "This SKS version has a minimum compatibility \
         requirement for recon of SKS %s\n"
      Common.compatible_version_string;
	
  printf "%s" dbstats_dir

