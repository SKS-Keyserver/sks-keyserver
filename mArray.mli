(***********************************************************************)
(* mArray.mli - Various array operations                               *)
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

val to_string : f:('a -> string) -> 'a array -> string
val print : f:('a -> 'b) -> 'a array -> unit

val all_true : bool array -> bool
val for_all : f:('a -> bool) -> 'a array -> bool
val exists : f:('a -> bool) -> 'a array -> bool
val mem : 'a -> 'a array -> bool
val choose_best : ('a -> 'a -> 'a) -> 'a array -> 'a
val max : 'a array -> 'a
val min : 'a array -> 'a
val count : f:('a -> bool) -> 'a array -> int
val count_true : bool array -> int
val average : float array -> float
val iaverage : int array -> float
val median : 'a array -> 'a
val zip : 'a array -> 'b array -> ('a * 'b) array
