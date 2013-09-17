(***********************************************************************)
(* utils.ml - A variety of simple utilities                            *)
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
module Set = PSet.Set
module Map = PMap.Map

open Printf

let compose f g x = f (g x)
let iceil x =  int_of_float (ceil x)
let ifloor x = int_of_float (floor x)

(** Binary search.

   (f i) returns -1, 0 or 1, and should be monotonic.
   f should have values for all i in [low,high], inclusive.

   if \E i \in [low,high] such that (f i) = 0,
   then such an i is returned.
   Otherwise, i is returned such that
   (f i = 1) and (f (i-1)=-1).
   Unless it's all 1's or all -1s.  If it's all 1s, the first 1 is returned.
   If it's all -1's, then raise Not_found
*)
let bsearch ~f ~low ~high =
  let rec bsearch ~f ~low ~high =
    if low = high then
      match f low with
          0 -> low
        | 1 -> low
        | _ -> raise Not_found
    else let mid = (low + high)/2 in
      match f mid with
          0 -> mid
        | 1 -> bsearch ~f ~low ~high:mid
        | (-1) -> bsearch ~f ~low:(mid+1) ~high
        | _ -> raise (Failure ("bsearch: " ^
                               "Search returned value other than -1,0,1"))
  in
    if high < low
    then raise Not_found
    else bsearch ~f ~low ~high

(** similar to bsearch, but returns (index,value) pair.
  f is expected to return a (test,value) pair,
  where test is like the output of f above, and value is some
  related value.  *)
let bsearch_val ~f ~low ~high =
  let rec bsearch_val ~f ~low ~high =
    (* print_string "."; flush stdout; *)
    if low = high then
      let (test,value) = f low in
        match test with
            0 -> (low,value)
          | 1 -> (low,value)
          | _ -> raise Not_found
    else
      let mid = (low + high)/2 in
      let (test,value) = f mid in
        match test with
            0 -> (mid,value)
          | 1 -> bsearch_val ~f ~low ~high:mid
          | (-1) -> bsearch_val ~f ~low:(mid+1) ~high
          | _ -> raise (Failure ("bsearch: " ^
                                 "Search returned value other than -1,0,1"))
  in
    if high < low
    then raise Not_found
    else bsearch_val ~f ~low ~high


(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

let is_alnum char =
  let num = int_of_char char in
  (num >= int_of_char 'A' && num <= int_of_char 'Z') ||
  (num >= int_of_char 'a' && num <= int_of_char 'z') ||
  (num >= int_of_char '0' && num <= int_of_char '9') ||
  (num >= 192 && num <= 255)


let rec extract_words_rec s ~start ~len partial =
  let one () = Set.add (String.lowercase (String.sub s start len)) partial in
  if start + len = String.length s
  then ( if len = 0 then partial
         else one ())
  else (
    if is_alnum s.[start + len]
    then extract_words_rec s ~start ~len:(len + 1) partial
    else ( if len = 0
           then extract_words_rec s ~start:(start + 1) ~len partial
           else extract_words_rec s ~start:(start + len)  ~len:0
             (one ())
         )
  )

(**  returns the set of words found in string s *)
let extract_word_set s =
  extract_words_rec s ~start:0 ~len:0 Set.empty

(** returns a list of words found in string s *)
let extract_words s =
  Set.elements (extract_word_set s)


(*******************************************************************)
(*  START: Miscellaneous  *****************************************)
(*******************************************************************)

(** print results of a test *)
let ptest str bool = match bool with
  true  -> printf "    Test %s passed\n" str; flush stdout
| false -> printf "*** Test %s FAILED ***" str; flush stdout

(** For all values i between first (incl) and last (excl) , evaluate func
   on i and partial *)
let rec for_loop first last partial func =
  if first = last
    then partial
    else for_loop (first+1) last (func first partial) func


(** For all pairs (i,j) of elements in list where i!=j and i < j, evaluate
   func (i,j) partial, building up partial as you go.  *)
let rec pair_loop func partial list = match list with
  [] -> partial
| i::tl ->
      let rec i_loop list partial = match list with
        [] -> partial
      | j::tl -> i_loop tl (func (i,j) partial) in
      pair_loop func (i_loop tl partial) tl

(** Note:  does not terminate upon finding a false instance *)
let for_all_pairs test list =
  let test_join (i,j) partial = (test i j) && partial in
  pair_loop test_join true list

let neq_test (x,y) partial =
  (x != y) && partial

let time func =
  let s_time = Unix.gettimeofday () in
  func ();
  (Unix.gettimeofday ()) -. s_time

let random_int low high =
  (Random.int (high-low)) + low

let char_width = 8

let hexstring digest =
  let result = String.create (String.length digest * 2) in
  let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = Char.code digest.[i] in
        result.[2*i] <- hex.[c lsr 4];
        result.[2*i+1] <- hex.[c land 0xF]
    done;
    result

let rec int_from_bstring_rec string ~pos ~len partial =
  if len = 0 then partial
  else
    int_from_bstring_rec string ~pos:(pos + 1) ~len:(len-1)
      ((partial lsl char_width) + (int_of_char string.[pos]))

let int_from_bstring string ~pos ~len =
  int_from_bstring_rec string ~pos ~len 0

let bstring_of_int i =
     let s = String.create 4 in
     s.[3] <- char_of_int (i land 0xFF);
     s.[2] <- char_of_int ((i lsr 8) land 0xFF);
     s.[1] <- char_of_int ((i lsr 16) land 0xFF);
     s.[0] <- char_of_int ((i lsr 24) land 0xFF);
     s

(* tail recursive *)
let rec apply count func start = match count with
  0 -> start
| _ -> apply (count-1) func (func start)

let get_bit ~pos i = (i lsr pos) land 1

let create_rand_bits () =
  let bits = ref (Random.bits ())
  and pos = ref 0 in
  let bitfunc () =
    if !pos > 30 then
      (pos := 0; bits := Random.bits ());
    let rval = get_bit ~pos:!pos !bits in
      pos := !pos + 1;
      rval
  in
    bitfunc

let rbit = create_rand_bits ()

(* FIX: this depends on the interals of the sort mechanism.
   A rather cheap trick, really. It does work at present, though *)
let permute list =
  let cmp i j = (rbit ()) * 2 - 1 in
    List.sort ~cmp list

(* Exception Handling *)

exception FinalDouble of exn * exn
exception Final of exn

let try_finally ~f ~finally =
  let finally_called = ref false in
  try
    let rval = f () in
    finally_called := true;
    finally ();
    rval
  with x ->
    if not !finally_called
    then
      begin
        (try finally () with
             y -> raise (FinalDouble (x,y)));
        raise x
      end

    else raise (Final x)


let rec rfold ~f low high ~init =
  if low >= high then init
  else (
    rfold ~f (low + 1) high ~init:(f init low)
  )

let rec fill_random_string rfunc string ~pos ~len =
  if pos < len then
    let steps =
      if len - pos > 3 then 3 else len - pos in
    (* CR yminsky: this is basically a bug.  We double-call rfunc for no reason.
       I'm worried about changing this because there is probably some assumptions about
       the random generation being deterministic *)
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

let dedup list = Set.elements (Set.of_list list)

(** returns memoized version of any fucntion with argument unit *)
let unit_memoize f =
  let store = ref None in
  (fun () ->
     match !store with
       | Some x -> x
       | None ->
           let rval = f () in
           store := Some rval;
           rval
  )

(** returns memoized version of any function with a single argument *)
let memoize f =
  let store = Hashtbl.create 10 in
  (fun x ->
     try Hashtbl.find store x
     with Not_found ->
       let rval = f x in
       Hashtbl.add store ~key:x ~data:rval;
       rval
  )

(** object-based memoizer. Main advantage here is that you can
  clear the cache. *)
class ['a] memo (f:'a) =
object (self)
  val store = Hashtbl.create 10

  method apply x =
    try Hashtbl.find store x
    with Not_found ->
      let rval = f x in
      Hashtbl.add store ~key:x ~data:rval;
      rval

  method clear = Hashtbl.clear store

end


let filter_map ~f list =
  let rec loop list accum = match list with
      [] -> List.rev accum
    | hd :: tl ->
        match f hd with
            None -> loop tl accum
          | Some x -> loop tl (x :: accum)
  in
  loop list []

let copy_conf src dst fn =
    let command = "cp " ^ (Filename.concat src fn) ^
      " " ^ (Filename.concat dst "DB_CONFIG")  in
    let r_command = Sys.command command in
    match r_command with
        | 0 -> ()
        | _ -> failwith ("Copy of DB_CONFIG failed")

let initdbconf src dst =
  let db = Filename.basename dst in
  let lstconf = ["DB_CONFIG." ^ db; "DB_CONFIG"] in
  let conf_exists conf = Sys.file_exists
        (Filename.concat src conf) in
  let found_conf = List.filter lstconf
    ~f:(fun x -> conf_exists x)  in
  match found_conf with
      [] -> ()
      | hd :: _ -> copy_conf src dst hd;
