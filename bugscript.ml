(***********************************************************************)
(* bugscript.ml                                                        *)
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

open Common
open StdLabels
open MoreLabels
open Printf
(*open Pstyle *)
module Set = PSet.Set
open ReconPTreeDb

(*
  #directory "/home/yminsky/Work/projects/keyserver/sks"
  #load "reconPTreeDb.cmo"
*)

let rec read_lines f accum =
      let line =
        try Some (input_line f)
        with End_of_file -> None
      in
      match line with
          Some line -> read_lines f (line::accum)
        | None -> List.rev accum

let read_lines f = read_lines f []

let entry_hash entry = match entry with
  | Add hash -> hash
  | Delete hash -> hash

let ch_piece ch pos line =
  if pos >= String.length line then raise Not_found;
  try
    let newpos = String.index_from line pos ch in
    (newpos+1,
     String.sub line ~pos ~len:(newpos - pos))
  with
      Not_found -> (String.length line,
                    String.sub line ~pos ~len:(String.length line - pos))

let rec ch_pieces ch pos line =
  let (newpos,piece) = ch_piece ch pos line in
  try piece::(ch_pieces ch newpos line)
  with Not_found -> piece::[]

let ws = Str.regexp " "

let line_to_entry line =
  let pieces = Array.of_list (ch_pieces ' ' 0 line) in
  let hash = KeyHash.dehexify pieces.(3) in
  match pieces.(2) with
    | "Add" -> Add hash
    | "Del" -> Delete hash
    | _ -> failwith "unparseable line"


(** compute the symmetric difference between two arrays
  sorted in increasing order
*)
let array_diff a1 a2 =
  let c1 = ref 0 and c2 = ref 0 in
  let diff1 = ref [] and diff2 = ref [] in

  let add1 () =
    diff1 := a1.(!c1)::!diff1;
    incr c1
  and add2 () =
    diff2 := a2.(!c2)::!diff2;
    incr c2
  in

  while !c1 < Array.length a1 || !c2 < Array.length a2 do
    if !c1 >= Array.length a1 then add2 ()
    else if !c2 >= Array.length a2 then add1 ()
    else if a1.(!c1) = a2.(!c2) then ( incr c1; incr c2; )
    else if a1.(!c1) < a2.(!c2) then add1 ()
    else add2 ()
  done;
  (List.rev !diff1,List.rev !diff2)


let rec read_entries f accum =
  let line =
    try Some (input_line f)
    with End_of_file -> None
  in
  match line with
      Some line -> read_entries f (line_to_entry line::accum)
    | None -> Array.of_list (List.rev accum)

let read_entries fname =
  let f = open_in fname in
  let run () =
    ignore (input_line f);
    read_entries f []
  in
  protect ~f:run ~finally:(fun () -> close_in f)

let get_entries fname =
  let f = open_in fname in
  let run () =
    let lines = read_lines f in
    let lines = Array.of_list lines in
    Array.map ~f:line_to_entry lines
  in
  protect ~f:run ~finally:(fun () -> close_in f)

let zz_of_hstr hstr =
     let hash = KeyHash.dehexify hstr in
     ZZp.of_bytes hash

let ptree_mem hstr =
    let zz = zz_of_hstr hstr in
    let rec loop depth =
      match (PTree.get_node ~sef:true !ptree zz depth).PTree.children with
          | PTree.Children _ -> loop (depth+1)
          | PTree.Leaf elements -> Set.mem (ZZp.to_bytes zz) elements
    in
    loop 0

let rec get_groups entries pos group accum =
  if pos >= Array.length entries then
    if group = [] then accum
    else group::accum
  else (
    match group with
      | [] -> get_groups entries (pos+1) [entries.(pos)] accum
      | group_hd::_ ->
          if entry_hash entries.(pos) = entry_hash group_hd
          then get_groups entries (pos+1) (entries.(pos)::group) accum
          else get_groups entries (pos+1) [entries.(pos)] (group::accum)
  )

let get_groups entries = get_groups entries 0 [] []

let rec last list = match list with
    [hd] -> hd
  | hd::tl -> last tl
  | [] -> raise Not_found

let simplify_groups groups =
  Array.of_list (List.rev_map ~f:last groups)

let bad_entry entry = match entry with
  | Add hash -> if ptree_mem hash then false else true
  | Delete hash -> if ptree_mem hash then true else false

let trunc s = String.sub ~pos:0 ~len:16 s

let get_ptree_hashes () =
  PTree.summarize_tree
    ~lagg:(fun set -> Array.map ~f:trunc
             (Array.of_list (Set.elements set)))
    ~cagg:(fun alist -> Array.concat (Array.to_list alist))
    !ptree

let lpush el lref = lref := el::!lref

let get_entry_droplist entries =
  let droplist = ref [] in
  for i = 0 to Array.length entries - 2 do
    if entry_hash entries.(i) = entry_hash entries.(i+1) then
      lpush i droplist
  done;
  List.rev !droplist

let dedup_entries entries =
  let droplist = get_entry_droplist entries in
  let drops = Set.of_list droplist in
  let new_entries = Array.make (Array.length entries - List.length droplist)
                      entries.(0)
  in
  let pos = ref 0 in
  for i = 0 to Array.length entries - 1 do
    if not (Set.mem i drops) then (
      new_entries.(!pos) <- entries.(i);
      incr pos
    )
  done;
  new_entries

let get_simplified_entries fname =
  perror "reading entries from log";
  let entries = read_entries fname in
  perror "sorting log entries";
  Array.stable_sort entries
    ~cmp:(fun x y -> compare (entry_hash x) (entry_hash y));
  perror "deduping log entries";
  dedup_entries entries

let count_adds entries =
  Array.fold_left ~init:0 entries
    ~f:(fun count entry -> match entry with
            Add hash -> count + 1
          | _ -> count)

let get_hashes simplified_entries =
  perror "extracting adds";
  let adds = count_adds simplified_entries in
  let hashes = Array.create adds "" in
  let pos = ref 0 in
  Array.iter simplified_entries
    ~f:(function Add hash ->
          hashes.(!pos) <- hash; incr pos
          | Delete hash -> ());
  hashes


let get_diffs () =
  let hashes = get_hashes (get_simplified_entries "log.real") in
  perror "Getting hashes from prefix tree...";
  let phashes = get_ptree_hashes () in

  perror "computing difference...";
  let (diff1,diff2) = array_diff hashes phashes in

  (Set.of_list diff1,Set.of_list diff2)

let rec line_iter ~f file =
  let line =
    try Some (input_line file)
    with End_of_file -> None
  in
  match line with
    | Some line -> f line; line_iter ~f file
    | None -> ()

let rewrite_log diff1 diff2 =
  let infile = open_in "log.real" in
  let outfile = open_out "log.real.annot" in
  output_string outfile (input_line infile);
  output_string outfile "\n";
  line_iter infile
    ~f:(fun line ->
          output_string outfile line;
          let entry = line_to_entry line in
          if Set.mem (entry_hash entry) diff1 then
            output_string outfile " <--- INLOG"
          else if Set.mem (entry_hash entry) diff2 then
            output_string outfile " <--- INPTR";
          output_string outfile "\n"
       );
  close_in infile;
  close_out outfile

let runtest () =
  let (diff1,diff2) = get_diffs () in
  perror "Rewriting log";
  rewrite_log diff1 diff2

let () = runtest ()

