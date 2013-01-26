(***********************************************************************)
(* pMap.mli - Association tables over ordered types.                   *)
(*                                                                     *)
(*            This module implements applicative association tables,   *)
(*            also known as finite maps or dictionaries, given a total *)
(*            ordering function over the keys.                         *)
(*            All operations over maps are purely applicative          *)
(*            (no side-effects).                                       *)
(*            The implementation uses balanced binary trees, and       *)
(*            therefore searching and insertion take time logarithmic  *)
(*            in the size of the map.                                  *)
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
    val compare : 'a -> 'a -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is
          the generic structural comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Map.Make}. *)

module type S =
  sig
    type ('key,'data) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: ('key,'data) t
    (** The empty map. *)

    val add: key:'key -> data:'data -> ('key,'data) t -> ('key,'data) t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val find: 'key -> ('key,'data) t -> 'data
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val remove: 'key -> ('key,'data) t -> ('key,'data) t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val mem: 'key -> ('key,'data) t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val iter: f:(key:'key -> data:'data -> unit) -> ('key,'data) t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument. The order in which the bindings are passed to
       [f] is unspecified. Only current bindings are presented to [f]:
       bindings hidden by more recent bindings are not passed to [f]. *)

    val map: f:('data -> 'a) -> ('key,'data) t -> ('key,'a) t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The order in which the associated values are passed to [f]
       is unspecified. *)

    val mapi: f:(key:'key -> data:'data -> 'a) ->
      ('key,'data) t -> ('key,'a) t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val fold: f:(key:'key -> data:'data -> 'a -> 'a) ->
      ('key,'data) t -> init:'a -> 'a
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m],
       and [d1 ... dN] are the associated data.
       The order in which the bindings are presented to [f] is
       unspecified. *)

    val of_alist: ('key * 'data) list -> ('key,'data) t
      (* [of_alist alist] converts the association list [alist] into
         the corresponding map *)

    val to_alist: ('key,'data) t -> ('key * 'data) list
      (* [of_alist map] converts the map [map] into
         the corresponding association list *)
  end
(** Output signature of the functor {!Map.Make}. *)

module Make (Ord : OrderedType) : S
(** Functor building an implementation of the map structure
   given a totally ordered type. *)

module Map : S
