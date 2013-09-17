(***********************************************************************)
(* pMap.ml - Association tables over ordered types.                    *)
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


module type OrderedType =
  sig val compare: 'a -> 'a -> int end

module ClassicalType =
  struct let compare = Pervasives.compare end

module type S =
  sig
    type ('key,'data) t
    val empty: ('key,'data) t
    val add: key:'key -> data:'data -> ('key,'data) t -> ('key,'data) t
    val find: 'key -> ('key,'data) t -> 'data
    val remove: 'key -> ('key,'data) t -> ('key,'data) t
    val mem:  'key -> ('key,'data) t -> bool
    val iter: f:(key:'key -> data:'data -> unit) -> ('key,'data) t -> unit
    val map: f:('data -> 'a) -> ('key,'data) t -> ('key,'a) t
    val mapi: f:(key:'key -> data:'data -> 'a) ->
      ('key,'data) t -> ('key,'a) t
    val fold: f:(key:'key -> data:'data -> 'a -> 'a) ->
      ('key,'data) t -> init:'a -> 'a
    val of_alist: ('key * 'data) list -> ('key,'data) t
    val to_alist: ('key,'data) t -> ('key * 'data) list
  end

module Make(Ord: OrderedType) = struct

    type ('key,'data) t =
        Empty
      | Node of ('key,'data) t * 'key * 'data * ('key,'data) t * int

    let empty = Empty

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let rec add ~key:x ~data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add ~key:x ~data l) v d r
          else
            bal l v d (add ~key:x ~data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (Node(l1, v1, d1, r1, h1), Node(l2, v2, d2, r2, h2)) ->
          bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter ~f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter ~f l; f ~key:v ~data:d; iter ~f r

    let rec map ~f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) -> Node(map ~f l, v, f d, map ~f r, h)

    let rec mapi ~f = function
        Empty               -> Empty
      | Node(l, v, d, r, h) ->
          Node(mapi ~f l, v, f ~key:v ~data:d, mapi ~f r, h)

    let rec fold ~f m ~init:accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold ~f l ~init:(f ~key:v ~data:d (fold ~f r ~init:accu))

    let of_alist alist =
      List.fold_left ~f:(fun map (key,data) -> add ~key ~data map)
        ~init:empty alist

    let to_alist map =
      fold ~f:(fun ~key ~data list -> (key,data)::list)
        ~init:[] map
end

module Map = Make(ClassicalType)
