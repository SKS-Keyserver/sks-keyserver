(***********************************************************************)
(* test.ml - Module for testing out the functionality of the           *)
(*           Berkeley DB interface                                     *)
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
module Unix = UnixLabels
open Printf
open Bdb
module SMap = Map.Make(struct type t = string let compare = compare end)
module Set = PSet.Set

exception TestFailed of string

let _ = Random.self_init ()

let chars = "abcdefghijklmnopqrstuvwxyz123456789"
let rand_string len =
  let s = String.create len in
  for i = 0 to String.length s - 1 do
    s.[i] <- chars.[Random.int (String.length chars)]
  done;
  s

let prepare_dir dirname =
  if MUnix.exists dirname then
    ignore (Unix.system (sprintf "rm -r %s" dirname));
  Unix.mkdir dirname

let prepare_file fname =
  if MUnix.exists fname then Unix.unlink fname


let simple_test () =
  let fname = "FOO" in
  prepare_file fname;
  let db = Db.sopen fname Db.HASH [ Db.CREATE ] 0o777 in
  let map = ref SMap.empty in
  for i = 0 to 1000 do
    let key = rand_string 5
    and data = rand_string 10
    in
    map := SMap.add key data !map;
    Db.put db ~key ~data []
  done;
  SMap.iter ~f:(fun ~key ~data ->
              let dbdata = Db.get db key [] in
              if dbdata <> data
              then raise (TestFailed "simple_test: values do not agree")) !map;
  SMap.iter ~f:(fun ~key ~data -> Db.del db key) !map;
  SMap.iter ~f:(fun ~key ~data ->
              try
                let dbdata = Db.get db key [] in
                raise (TestFailed "simple_test: deleted value found anyway")
              with
                  Not_found -> ()
           ) !map;
  print_string "Simple Test passed\n"


let leak_test () =
  let size = 10000 in
  for i = 1 to size do
    let x = Dbenv.create [] in
    Dbenv.close x
  done;
  for i = 1 to size do
    let x = Db.create [] in
    Db.close x
  done;
  for i = 1 to size do
    let x = Db.create [] in
    Db.close x
  done;
  let fname = "FOO" in
  prepare_file fname;
  let db = Db.sopen fname Db.BTREE [ Db.CREATE ] 0o777 in
  for i = 1 to size do
    let x = Cursor.create db in
    Cursor.close x
  done;
  print_string "Leak Test completed\n"

let cursor_get_all c =
  let rec loop list =
    try loop (Cursor.get c Cursor.NEXT_DUP [] :: list)
    with Not_found -> list
  in
  let first = Cursor.get c Cursor.CURRENT [] in
  loop [first]

let jcursor_get_all c =
  let rec loop list =
    match (try Some (Cursor.get c Cursor.NULL [])
           with Not_found -> None)
    with
        Some (key,data) -> loop (data::list)
      | None -> list
  in
  loop []

let cursor_test () =
  let idbname = "FOO" and pdbname = "BAR" in
  prepare_file idbname; prepare_file pdbname;
  let idb = Db.sopen idbname Db.HASH
             ~moreflags:[Db.DUP] [ Db.CREATE ] 0o777 in
  let pdb = Db.sopen pdbname Db.HASH [ Db.CREATE ] 0o777 in
  let ci = Cursor.create idb and cp = Cursor.create pdb in
  let common =
    Set.of_list (MList.init 10 ~f:(fun i -> rand_string 30)) in
  let s1 = Set.union common
             (Set.of_list (MList.init 10 ~f:(fun i -> rand_string 30)))
  and s2 = Set.union common
             (Set.of_list (MList.init 10 ~f:(fun i -> rand_string 30)))
  and s3 = Set.union common
             (Set.of_list (MList.init 10 ~f:(fun i -> rand_string 30)))
  and key1 = rand_string 10
  and key2 = rand_string 10
  and key3 = rand_string 10
  in
  Set.iter ~f:(fun data ->
                 Cursor.kput cp ~key:data ~data:data Cursor.KEYLAST;
                 Cursor.kput ci ~key:key1 ~data:data Cursor.KEYLAST) s1;
  Set.iter ~f:(fun data ->
                 Cursor.kput cp ~key:data ~data:data Cursor.KEYLAST;
                 Cursor.kput ci ~key:key2 ~data:data Cursor.KEYLAST) s2;
  Set.iter ~f:(fun data ->
                 Cursor.kput cp ~key:data ~data:data Cursor.KEYLAST;
                 Cursor.kput ci ~key:key3 ~data:data Cursor.KEYLAST) s3;
  Cursor.close cp;
  Cursor.close ci;

  let c1 = Cursor.create idb
  and c2 = Cursor.create idb
  and c3 = Cursor.create idb in
  ignore (Cursor.init c1 key1 []);
  ignore (Cursor.init c2 key2 []);
  ignore (Cursor.init c3 key3 []);
  let cj = Cursor.join pdb [c1;c2;c3] [] in
  let jcommon = Set.of_list (jcursor_get_all cj) in
  (*
    let rs1 = Set.of_list (cursor_get_all c1)
    and rs2 = Set.of_list (cursor_get_all c2)
    and rs3 = Set.of_list (cursor_get_all c3) in
    let rcommon = Set.inter (Set.inter rs1 rs2) rs3 in
  *)
  if not (Set.equal jcommon common)
  then raise (TestFailed "sets not equal");
  print_string "Cursor Test passed\n"



  let _ =
    simple_test ();
    leak_test ();
    cursor_test ()
