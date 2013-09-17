(***********************************************************************)
(* dbscript.ml                                                         *)
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

module Kdb = Keydb.MakeUnrestricted(
  struct
    let withtxn = !Settings.transactions
    and cache_bytes = !Settings.cache_bytes
    and pagesize = !Settings.pagesize
    and dbdir = "/usr/share/keyfiles/sks_the_2/KDB"
    and dumpdir = "/usr/share/keyfiles/sks_the_2/dump"
  end
)



(*
let unwrap x = match x with Some x -> x | None -> failwith "unwrapping None"
let () = Keydb.open_dbs ()
let (stream,close) = Keydb.create_hashstream ()


let weirdhash_str = "C2A6E1C3749690E04AC6AFC2A2679A4E"
let weirdhash = KeyHash.dehexify weirdhash_str
let last = ref ""
let x =
  while
    last := (unwrap (SStream.next stream));
    !last < weirdhash
  do () done
*)
