(***********************************************************************)
(* mList.ml - Various list operations                                  *)
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

(************************************************************************)
(* START: List Operations **********************************************)
(************************************************************************)

(****** Numeric *************)
let average list =
  let sum = List.fold_left ~f:(+.) ~init:0.0 list in
    sum /. (float_of_int (List.length list))

let iaverage list =
  let sum = List.fold_left ~f:(+) ~init:0 list in
    (float sum) /. (float (List.length list))


(****** Initialization *************)

let init n ~f =
  let rec list_init ~n ~partial =
    match n with
        0 -> partial
      | _ -> list_init ~n:(n-1) ~partial:((f (n-1))::partial)
  in list_init ~n ~partial:[]

let init_by_value n ~value =
  let rec init_list_rec n value partial = match n with
    0 -> partial
  | _ -> init_list_rec (n - 1) value (value::partial)
  in init_list_rec n value []


(******** Printing *****************)

let to_string ~f list =
  let buf = Buffer.create ((List.length list) * 5) in
    Buffer.add_string buf "[ ";
    List.iter ~f:(fun el ->
                    Buffer.add_string buf (f el);
                    Buffer.add_string buf " "; )
      list;
    Buffer.add_string buf  "]";
    Buffer.contents buf

let print ~f list =
  let rec print_list_rec list = match list with
      [] -> ()
    | hd::tl ->
        f hd;
        print_string " ";
        print_list_rec tl
  in
    print_string "[ ";
    print_list_rec list;
    print_string "]"

let print_int_list = print ~f:(printf "%d ")

let print2 ~f list =
  let rec print_list_rec list = match list with
      [] -> ()
    | hd::tl ->
        f hd;
        print_string "\n  ";
        print_list_rec tl
  in
    print_string "[ ";
    print_list_rec list;
    print_string " ]"

(***********************************************)



let rec swap_pairs_rec list  partial = match list with
  [] -> partial
| (a,b)::tail -> swap_pairs_rec tail ( (b,a)::partial )

let swap_pairs list = swap_pairs_rec list []

(* tail recursive, constructs list from
   lower_bound (incl) to upper_bound (excl) *)
let range lower_bound upper_bound =
  let rec range_rec lower_bound upper_bound list =
    if lower_bound = upper_bound
      then list
      else range_rec lower_bound (upper_bound-1) ((upper_bound -1)::list)
  in range_rec lower_bound upper_bound []

let srange ?(step=1) lower_bound upper_bound =
  let rec range lower_bound partial =
    if lower_bound >= upper_bound
    then partial
    else range (lower_bound + step) (lower_bound::partial)
  in List.rev(range lower_bound [])

let rand_elem list =
  if (List.length list) = 0
    then raise (Failure "attempt to select random element of empty list")
    else List.nth list (Random.int (List.length list))

(* return list with first element dropped *)
let omit_first list = match list with
  [] -> raise (Failure "attempt to drop element from empty list")
| hd::tl -> tl;;

(* return list with kth element dropped *)
let rec drop_kth ~k list = match list, k with
    [],_ -> []
  | list,0  -> omit_first list
  | hd::tail,k -> hd::(drop_kth ~k:(k-1) tail)

(* return list with only the first k elements *)
let first_k ~k list =
  let rec first_k_rec list k partial = match list,k with
      [],_ -> partial
    | _,0  -> partial
    | hd::tl,k -> first_k_rec tl (k-1) (hd::partial)
  in List.rev (first_k_rec list k [])

let k_split ~k ~list =
  let rec k_split ~k part1 part2 =
    if k = 0 then (part1, part2)
    else (
      match part2 with
          [] -> (part1,[])
        | hd::tail ->  k_split ~k:(k-1) (hd::part1) tail
    )
  in
  let (part1, part2) = k_split ~k [] list
  in (List.rev part1, part2)


let rec last_elem list = match list with
    [] -> raise (Failure "Attempt to get end of empty list")
  | [hd] -> hd
  | hd::tl -> last_elem tl

let rec last_k ~k list =  match list with
  [] -> []
| hd::tl -> if k >= (List.length list)
    then list
    else last_k tl ~k

(* return list with all but first k *)
let rec drop_k ~k list = match list, k with
  [],_ -> []
| list,0 -> list
| hd::tail,k -> drop_k tail ~k:(k-1)

let drop_last_k ~k list =
  let rec drop_rec list k partial =
    if (List.length list) <= k
        then partial
        else match list with
          [] -> raise (Failure "drop_last_k: Unexpected error")
        | hd::tl -> drop_rec tl k (hd::partial)
  in List.rev (drop_rec list k [])

let drop_last list = drop_last_k ~k:1 list

let all_true list =
  List.fold_left ~f:(fun a b -> a && b) ~init:true list

let pri_split pri list =
  let rec pri_split_rec list low exact high = match list with
    [] -> (low,exact,high)
  | ((el_pri,_) as hd)::tl ->
        if el_pri < pri then pri_split_rec tl (hd::low) exact high
          else if el_pri > pri then pri_split_rec tl low exact (hd::high)
            else pri_split_rec tl low (hd::exact) high
  in let (low,exact,high)= pri_split_rec list [] [] [] in
  assert ( (List.length low) + (List.length exact) + (List.length high) =
             (List.length list) );
  (low,exact,high)

let has_dups list =
  let slist = Sort.list (fun x y -> x < y) list in
  let rec dup_scan list = match list with
    [] -> false
  | hd::[] -> false
  | hd1::hd2::tl -> if hd1 = hd2 then true else dup_scan (hd2::tl)
  in dup_scan slist

let dedup list =
  let slist = Sort.list (fun x y -> x < y) list in
  let rec dedup ~list ~partial = match list with
      [] -> partial
    | hd::[] -> dedup ~list:[] ~partial:(hd::partial)
    | hd1::hd2::tl ->
        if hd1 = hd2
        then dedup ~list:(hd2::tl) ~partial
        else dedup ~list:(hd2::tl) ~partial:(hd1::partial)
  in List.rev (dedup ~list:slist ~partial:[]);;

let choose_best ~f:best_chooser list =
  let rec choose_best ~list best_so_far =
    match list with
        [] -> best_so_far
      | hd::tl -> choose_best ~list:tl (best_chooser hd best_so_far)
  in match list with
      [] -> raise (Failure "Attempt to get best element of empty list")
    | hd::tl -> choose_best ~list:tl hd

let count_true list =
  let rec count_true list partial = match list with
      [] -> partial
    | hd::tl -> count_true tl (partial + if hd then 1 else 0)
  in count_true list 0

let max list = choose_best ~f:max list
let min list = choose_best ~f:min list


(******************************************************)
(*** Some functions that should be in module List ... *)
(******************************************************)

(* UNTESTED *)
let rec iteri_rec ~f list i = match list with
    [] -> ()
  | hd::tl -> f ~i hd; iteri_rec ~f tl (i+1)

let iteri ~f list =
  iteri_rec ~f list 0

(******************************************************)

(* UNTESTED *)
let rec mapi_rec ~f list i partial = match list with
    [] -> partial
  | hd::tl -> mapi_rec ~f tl (i+1)  ((f ~i hd)::partial)

let mapi ~f list =
  List.rev (mapi_rec ~f list 0 [])

(******************************************************)

let map ~f list = List.rev (List.rev_map ~f list)

(******************************************************)

(* UNTESTED *)
let rec filteri_rec ~f list i partial = match list with
    [] -> partial
  | hd::tl ->
      if f ~i hd
      then filteri_rec ~f tl (i+1) (hd::partial)
      else filteri_rec ~f tl (i+1) partial

let filteri ~f list =
  List.rev (filteri_rec ~f list 0 [])

(******************************************************)

let find_index el list =
  let rec find_index list loc = match list with
      [] -> -1
    | hd::tl ->
        if hd = el then loc
        else find_index tl (loc + 1)
  in
    find_index list 0

let cons_opt opt list =  match opt with
    None -> list
  | Some x -> x::list

let strip_opt list =
  let rec loop list stripped =  match list with
      [] -> List.rev stripped
    | None::tl -> loop tl stripped
    | (Some x)::tl -> loop tl (x::stripped)
  in
    loop list []

let rec reduce ~f list = match list with
      [] -> failwith "MList.reduce: list has two few elements"
    | hd::tl -> List.fold_left ~f tl ~init:hd



