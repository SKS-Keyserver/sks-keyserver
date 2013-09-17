(***********************************************************************)
(* mList.mli - Various list operations                                 *)
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

val average : float list -> float
val iaverage : int list -> float
val init : int -> f:(int -> 'a)  -> 'a list
val init_by_value : int -> value:'a  -> 'a list

val to_string : f:('a -> string) -> 'a list -> string
val print_int_list : int list -> unit
val print : f:('a -> 'b) -> 'a list -> unit
val print2 : f:('a -> 'b) -> 'a list -> unit

val swap_pairs_rec : ('a * 'b) list -> ('b * 'a) list -> ('b * 'a) list
val swap_pairs : ('a * 'b) list -> ('b * 'a) list
val range : int -> int -> int list
val srange : ?step:int -> int -> int -> int list
val rand_elem : 'a list -> 'a
val omit_first : 'a list -> 'a list
val drop_kth : k:int -> 'a list -> 'a list
val first_k : k:int -> 'a list -> 'a list
val k_split : k:int -> list:'a list -> 'a list * 'a list
val last_elem : 'a list -> 'a
val last_k : k:int -> 'a list -> 'a list
val drop_k : k:int -> 'a list -> 'a list
val drop_last_k : k:int -> 'a list -> 'a list
val drop_last : 'a list -> 'a list
val all_true : bool list -> bool
val pri_split :  'a -> ('a * 'b) list -> ('a * 'b) list * ('a * 'b) list * ('a * 'b) list
val has_dups : 'a list -> bool
val dedup : 'a list -> 'a list
val choose_best : f:('a -> 'a -> 'a) -> 'a list -> 'a
val count_true : bool list -> int
val max : 'a list -> 'a
val min : 'a  list -> 'a

val iteri : f:(i:int -> 'a -> 'b) -> 'a list -> unit
val mapi : f:(i:int -> 'a -> 'b) -> 'a list -> 'b list
val map : f:('a -> 'b) -> 'a list -> 'b list
val filteri : f:(i:int -> 'a -> bool) -> 'a list -> 'a list

val find_index : 'a -> 'a list -> int
val cons_opt : 'a option -> 'a list -> 'a list
val strip_opt : 'a option list -> 'a list
val reduce : f : ( 'a -> 'a -> 'a ) -> 'a list -> 'a
