(***********************************************************************)
(* heap.ml - Simple heap implementation, adapted from CLR              *)
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

(* Adapted from CLR *)

type ('key,'data) heap_el = { key: 'key;
                              data: 'data;
                            }

type ('key,'data) heap = { mutable a: ('key,'data) heap_el option array;
                           mutable length: int;
                           minsize: int;
                           cmp: 'key -> 'key -> bool;
                         }

let length heap = heap.length
let true_length heap = Array.length heap.a

(***************************************************************)

let parent i = (i-1)/2
let left i = 2 * i + 1
let right i = 2 * i + 2
let get heap i = match heap.a.(i) with
    None -> raise (Failure "Heap.get: Attempt to examine None")
  | Some el -> el

let exchange heap i j =
  let temp = heap.a.(i) in
    heap.a.(i) <- heap.a.(j);
    heap.a.(j) <- temp

(***************************************************************)

let resize heap =
  if heap.length > Array.length heap.a
  then heap.a <-
    Array.init ((Array.length heap.a) * 2)
    ~f:(fun i ->
          if i < (Array.length heap.a)
          then heap.a.(i)
          else None)

  else
    if heap.length <= (Array.length heap.a)/3
      && (Array.length heap.a)/2 >= heap.minsize
    then heap.a <-
      Array.init ((Array.length heap.a)/ 2) ~f:(fun i -> heap.a.(i))


(***************************************************************)

let rec heapify heap i =
  let left = left i in
  let right = right i in
  let largest =
    if left < heap.length &&
      heap.cmp (get heap left).key (get heap i).key
    then left else i in
  let largest =
    if right < heap.length &&
      heap.cmp (get heap right).key (get heap largest).key
    then right
    else largest
  in
    if i <> largest then
      begin
        exchange heap i largest;
        heapify heap largest
      end

(***************************************************************)

let build_heap_from_array cmp array length =
  let heap = { a = array;
               length = length;
               minsize = length;
               cmp = cmp
             }
  in
  let rec loop i =
    heapify heap i;
    loop (i-1)
  in
    loop (parent length)

(***************************************************************)

let top heap = match heap.length with
    0 -> raise Not_found
  | _ -> let max = get heap 0 in
      (max.key, max.data)


(***************************************************************)

let rec pop heap = match heap.length with
    0 -> raise Not_found;
  | _ -> let max = (get heap 0) in
      heap.a.(0) <- heap.a.(heap.length - 1);
      heap.length <- (heap.length - 1);
      heapify heap 0;
      resize heap;
      (max.key, max.data)


(***************************************************************)

let push heap ~key ~data =
  heap.length <- (heap.length + 1);
  resize heap;
  let rec loop i =
    if i > 0 && heap.cmp key (get heap (parent i)).key then
      begin
        heap.a.(i) <- heap.a.(parent i);
        loop (parent i)
      end
    else i
  in
  let i = loop (heap.length - 1) in
    heap.a.(i) <- Some { key = key; data = data; }


(***************************************************************)

let empty cmp i =
  { a = Array.create i None;
    length = 0;
    minsize = i;
    cmp = cmp;
  }

