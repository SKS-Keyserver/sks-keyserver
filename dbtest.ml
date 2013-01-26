(***********************************************************************)
(* dbtest.ml                                                           *)
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

module Kdb = Keydb.Unsafe

let settings = {
  Keydb.withtxn = !Settings.transactions;
  Keydb.cache_bytes = !Settings.cache_bytes;
  Keydb.pagesize = !Settings.pagesize;
  Keydb.dbdir = "/usr/share/keyfiles/sks_blackhole/KDB";
  Keydb.dumpdir = "/usr/share/keyfiles/sks_blackhole/dump";
}
let () = Kdb.open_dbs settings

let rec strip_opt list = match list with
    [] -> []
  | None::tl -> strip_opt tl
  | (Some hd)::tl -> hd::(strip_opt tl)


let rec beginning n list =
  if n = 0 then []
  else match list with
      [] -> []
    | hd::tl -> hd::(beginning (n-1) tl)

let merge_all keys =
  let keys = Array.to_list keys in
  match keys with
      hd::tl ->
        List.fold_left ~init:hd tl
        ~f:(fun key1 key2 -> match KeyMerge.merge key1 key2 with
                None -> failwith "hit unparseable key"
              | Some key -> key)
    | [] -> failwith "List too short"

let mergeable key1 key2 =
  match KeyMerge.merge key1 key2 with
      None -> false
    | Some key -> true

exception KeyFail of string

let ctr = ref 0
let click () =
  incr ctr;
  if !ctr mod 100 = 0
  then (
    printf "%d\n" !ctr;
    flush stdout;
  )

