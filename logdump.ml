(***********************************************************************)
(* logdump.ml                                                          *)
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
open Packet
module Unix = UnixLabels
open Unix
open DbMessages

module Keydb = Keydb.Make(struct
                            let withtxn = !Settings.transactions
                            and cache_bytes = !Settings.cache_bytes
                            and pagesize = !Settings.pagesize
                            and dbdir = !Settings.dbdir
                            and dumpdir = !Settings.dumpdir
                          end)

let print_entry (time,event) =
  let tm = Unix.localtime time in
  printf "%04d-%02d-%02d %02d:%02d:%02d "
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday (* date *)
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
  (match event with
     | Add hash -> printf "Add %s" (KeyHash.hexify hash)
     | Delete hash -> printf "Del %s" (KeyHash.hexify hash)
  );
  printf "\n"

let rec last list = match list with
    [] -> raise Not_found
  | [x] -> x
  | hd::tl -> last tl

let rec printlog ts =
  let entries = Keydb.logquery ts in
  if entries = [] then ()
  else
    let (new_ts,_) = last entries in
    List.iter entries ~f:print_entry;
    printlog new_ts


let () =
  Keydb.open_dbs ();
  printlog 0.
