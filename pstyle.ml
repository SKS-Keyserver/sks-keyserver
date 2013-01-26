(***********************************************************************)
(* pstyle.ml - Allows for some python-like tricks, at the expense of   *)
(*             some performance and indirection                        *)
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

module Array =
struct
  include Array
  let normalize ar i = if i < 0 then length ar + i else i
  let get ar i = get ar (normalize ar i)
  let slice start stop ar =
    let stop = if stop = 0 then length ar else stop in
    let pos = normalize ar start in
    let len = (normalize ar stop) - pos in
    sub ar ~pos ~len
end

module String =
struct
  include String
  let normalize str i = if i < 0 then length str + i else i
  let get str i = get str (normalize str i)
  let slice start stop str =
    let stop = if stop = 0 then length str else stop in
    let pos = normalize str start in
    let len = (normalize str stop) - pos in
    sub str ~pos ~len
end

let rec range ?(stride=1) ?(start=0) stop =
  if start >= stop then []
  else start::(range ~stride ~start:(start+stride) stop)


let ( </> ) string (start,stop) = String.slice start stop string
let ( <|> ) ar (start,stop) = Array.slice start stop ar
