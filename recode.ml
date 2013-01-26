(***********************************************************************)
(* recode.ml                                                           *)
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
open StdLabels
open MoreLabels

open Packet

let limit = try int_of_string Sys.argv.(1) with _ -> 10
let cin = new Channel.sys_in_channel stdin
let cout = new Channel.sys_out_channel stdout
let getkey = Key.get_of_channel cin

let _ =
  let count = ref 0 in
  ( try
      while !count < limit do
        Key.write (getkey ()) cout;
        incr count
      done
    with
        Not_found -> () )
