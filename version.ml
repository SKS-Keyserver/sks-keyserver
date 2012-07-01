(***********************************************************************)
(* version.ml - Executable: Show version information                   *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012  Yaron Minsky and Contributors             *)
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
open Bdb

let run() =  
   printf "SKS version %s%s\nThis version has a minimum compatibility requirement for recon of SKS %s\n" Common.version Common.version_suffix Common.compatible_version_string; 
   let bdb_version = version() in 
   printf "Compiled with BDB version %s\n" bdb_version;
   
   let sopen dirname flags mode = 
     let dbenv = Dbenv.create () in
     Dbenv.dopen dbenv dirname flags mode;
     dbenv
   in
   let dbenv = sopen (Lazy.force Settings.dbdir) [Dbenv.CREATE] 0o400 in
   let stats = Dbenv.get_dbenv_stats(dbenv);  in 
     printf "Detailed BDB environment statistics:\n%s\n" stats;

   let bdb_version = version() in
   match (Str.split (Str.regexp_string ".") bdb_version) with
   | major::minor::_ -> printf "Further details can be seen by executing db%s.%s_stat -x in the KDB and Ptree directories\n" major minor;
   | [] | _::[] -> printf "Further details can be seen by executing db%s.%s_stat -x in the KDB and Ptree directories\n" "X" "Y";