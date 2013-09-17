(***********************************************************************)
(* pSet.mli - Sets over ordered types                                  *)
(*                                                                     *)
(*            This module implements the set data structure, given a   *)
(*            total ordering function over the set elements.           *)
(*            All operations over sets are purely applicative          *)
(*            (no side-effects).                                       *)
(*            The implementation uses balanced binary trees, and is    *)
(*            therefore reasonably efficient: insertion and membership *)
(*            take time logarithmic in the size of the set, for        *)
(*            instance.                                                *)
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

module type OrderedType =
  sig
    val compare : 'elt -> 'elt -> int
      (** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is
          the generic structural comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Set.Make}. *)

module type S =
  sig

    type 'elt t
    (** The type of sets. *)

    val empty: 'elt t
    (** The empty set. *)

    val is_empty: 'elt t -> bool
    (** Test whether a set is empty or not. *)

    val mem: 'elt -> 'elt t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: 'elt -> 'elt t -> 'elt t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

    val singleton: 'elt -> 'elt t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: 'elt -> 'elt t -> 'elt t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

    val union: 'elt t -> 'elt t -> 'elt t
    (** Set union. *)

    val inter: 'elt t -> 'elt t -> 'elt t
    (** Set interseection. *)

    (** Set difference. *)
    val diff: 'elt t -> 'elt t -> 'elt t

    val compare: 'elt t -> 'elt t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: 'elt t -> 'elt t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: 'elt t -> 'elt t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: f:('elt -> unit) -> 'elt t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The order in which the elements of [s] are presented to [f]
       is unspecified. *)

    val fold: f:('elt -> 'a -> 'a) -> 'elt t -> init:'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s].
       The order in which elements of [s] are presented to [f] is
       unspecified. *)

    val for_all: f:('elt -> bool) -> 'elt t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: f:('elt -> bool) -> 'elt t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: f:('elt -> bool) -> 'elt t -> 'elt t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val partition: f:('elt -> bool) -> 'elt t -> 'elt t * 'elt t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: 'elt t -> int
    (** Return the number of elements of a set. *)

    val elements: 'elt t -> 'elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val min_elt: 'elt t -> 'elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val max_elt: 'elt t -> 'elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val choose: 'elt t -> 'elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val of_list: 'elt list -> 'elt t
    (** Returns a set constructed from the list elements *)

  end
(** Output signature of the functor {!Set.Make}. *)

module Make (Ord : OrderedType) : S
(** Functor building an implementation of the set structure
   given a totally ordered type. *)

module Set : S
