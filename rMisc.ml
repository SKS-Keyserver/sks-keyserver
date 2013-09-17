(***********************************************************************)
(* rMisc.ml - Miscellaneous utilities associated with reconciliation,  *)
(*            and in particular those that require access to the size  *)
(*            of the prime modulus.                                    *)
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
module Unix=UnixLabels

(** deterministic RNG *)
let det_rng = Random.State.make [|104|]
module Set = PSet.Set (* was: Polyset.Set *)
module Map = PMap.Map

let stringset_to_string stringset =
  let list = List.sort ~cmp:compare (Set.elements stringset) in
  let cout = Channel.new_buffer_outc 1024 in
    List.iter ~f:(fun string ->
                    cout#write_int (String.length string);
                    cout#write_string string)
      list;
    cout#contents

let digest_stringset strings =
  let string = stringset_to_string strings in
    Digest.string string

let print_lengths list =
  let list = List.sort ~cmp:compare list in
  MList.print ~f:(fun s -> Printf.printf "%d" (String.length s))
    list

let rec fill_random_string rfunc string ~pos ~len =
  if pos < len then
    let steps =
      if len - pos > 3 then 3 else len - pos in
    (* CR yminsky: I think this has the same bug as the function with the same name in Utils *)
    let _bits = rfunc () in
      for i = 0 to steps - 1 do
        string.[pos + i] <-
        char_of_int (0xFF land ((rfunc ()) lsr (8 * i)))
      done;
      fill_random_string rfunc string ~pos:(pos + steps) ~len
  else
    ()

let random_string rfunc len =
  let string = String.create len in
    fill_random_string rfunc string ~pos:0 ~len;
    string

let conv_chans (cin, cout) =
  (new MeteredChannel.metered_in_channel (new Channel.sys_in_channel cin),
   new MeteredChannel.metered_out_channel (new Channel.sys_out_channel cout))
(*    new Bufchan.buf_out_channel cout (1024 * 100)) *)
(************************************************************)
(* String Sets  ********************************************)
(************************************************************)

let add_random rfunc bytelength set =
  Set.add (random_string rfunc bytelength) set

let add_n_random rfunc bytelength ~n set =
  Utils.apply n (add_random rfunc bytelength) set

let det_string_set ~bytes ~size =
  add_n_random
    (fun () -> Random.State.bits det_rng)
    bytes ~n:size Set.empty

let rand_string_set ~bytes ~size =
  add_n_random Random.bits bytes ~n:size Set.empty

let localize_string_set ~bytes ~diff set =
  add_n_random Random.bits bytes ~n:diff set

(*
let local_string_set ~bytes ~base_size ~diff =
  let base_set = det_string_set ~bytes ~size:base_size in
  let local_set = add_n_random Random.bits bytes ~n:diff base_set in
    local_set
*)

(*
let string_sets ~bytes ~base_size ~diff =
  let base_set = det_string_set ~bytes ~size:base_size in
  let diff_set = add_n_random Random.bits bytes ~n:diff Set.empty in
  (base_set,diff_set)
*)

(*
let print_string_set set =
  let list = Set.elements set in
  let list= List.sort ~cmp:compare list in
  List.iter ~f:(fun string -> print_string string; print_newline ())
*)

let add_sarray ~data sarray =
  Array.fold_right ~f:(fun string set -> Set.add string set)
    sarray ~init:data

(*****************************************************************)
(*****************************************************************)

let pad string bytes =
  let len = String.length string in
  if bytes > len then
    let nstr = String.create bytes in
    String.fill nstr ~pos:len ~len:(bytes - len) '\000';
    String.blit ~src:string ~dst:nstr ~src_pos:0 ~dst_pos:0 ~len;
    nstr
  else
    string


let padset stringset bytes =
  Set.fold ~f:(fun el set -> Set.add (pad el bytes) set)
    ~init:Set.empty stringset

let truncate string bytes =
  let len = String.length string in
  if bytes < len then
    let nstr = String.create bytes in
    String.blit ~src:string ~dst:nstr ~src_pos:0 ~dst_pos:0 ~len:bytes;
    nstr
  else
    string

let truncset stringset bytes =
  Set.fold ~f:(fun el set -> Set.add (truncate el bytes) set)
    ~init:Set.empty stringset



(*****************************************************************)
(*  PRIMENESS-RELATED THINGS  ***********************************)
(*****************************************************************)

let order_string = "530512889551602322505127520352579437339"

(** Printing Functions *)

let print_ZZp_list list =
  let list = Sort.list (fun x y -> compare x y < 0) list in
  MList.print2 ~f:ZZp.print list

let print_ZZp_set set = print_ZZp_list (Set.elements set)


(*************  Initialization code ****************************)

let _ =
  Settings.setup_RNG ();
  ZZp.set_order (ZZp.of_string order_string)

