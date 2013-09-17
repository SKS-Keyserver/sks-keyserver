(***********************************************************************)
(* mArray.ml - Various array operations                                *)
(*                                                                     *)
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


let to_string ~f array =
  let buf = Buffer.create ((Array.length array) * 5) in
    Buffer.add_string buf "[| ";
    Array.iter ~f:(fun el ->
                    Buffer.add_string buf (f el);
                    Buffer.add_string buf " "; )
      array;
    Buffer.add_string buf  "|]";
    Buffer.contents buf

let print ~f array =
  print_string "[| ";
  Array.iter ~f:(fun el ->
                   f el;
                   print_string " ")

    array;
  print_string "|]"



(************************************************************************)
(* START: Array Operations *********************************************)
(************************************************************************)

let all_true array =
  Array.fold_left ~f:(&&) ~init:true array

let for_all ~f:test array =
  Array.fold_left ~f:(fun a b -> a && (test b)) ~init:true array

let exists ~f:test array =
  Array.fold_left ~f:(fun a b -> a || (test b)) ~init:false array

let mem el array =
  let length = Array.length array in
  let rec mem i el array =
    if i >= length then false
    else if el = array.(i) then true
    else mem (i+1) el array
  in mem 0  el array


let choose_best best_chooser array =
  let n = Array.length array in
  let rec choose_best ~i ~best =
    if i = n then best
    else choose_best ~i:(i+1) ~best:(best_chooser best array.(i))
  in
    if Array.length array < 1
    then raise (Failure "Attempt to get best element of empty array")
    else choose_best ~i:1 ~best:array.(0)

let max ar = choose_best max ar
let min ar = choose_best min ar

let count ~f array =
  Array.fold_left ~f:(fun count el ->
                        if f el then count + 1
                        else count)
    ~init:0 array

let count_true array =
  let n = Array.length array in
  let rec count_true array ~i ~partial =
    if i >= n then partial
    else count_true array ~i:(i+1)
      ~partial:(if array.(i) then partial + 1 else partial)
  in count_true array ~i:0 ~partial:0

let average array =
  let sum = Array.fold_left ~f:(+.) ~init:0.0 array in
    sum /. (float_of_int (Array.length array))

let iaverage array =
  average (Array.map ~f:(fun i -> float_of_int i) array)

let median array =
  let n = Array.length array in
  let sorted_array = Array.copy array in
    Array.stable_sort ~cmp:compare sorted_array;
    array.(n/2)

let zip array1 array2 =
  if Array.length array1 <> Array.length array2
  then failwith "Zipping arrays of different lengths"
  else Array.init (Array.length array1) ~f:(fun i -> (array1.(i), array2.(i)))


