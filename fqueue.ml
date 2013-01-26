(***********************************************************************)
(* fqueue.ml - Simple implementation of a polymorphic functional queue *)
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


(** push and top are O(1).
   pop and take are O(1) amortized.
   to_list and length are O(n).
*)

(* Invariant:
   if queue is not empty, outlist is not empty
   queue.length = List.length(queue.outlist) + List.length(queue.inlist)*)

exception Empty

type 'a t = { inlist: 'a list;
              outlist: 'a list;
              length: int;
            }

(*****************************************)

(*
let test_invariants queue =
  assert
    begin
      queue.length = (List.length queue.outlist) + (List.length queue.inlist)
    end;
  assert
    begin
      (queue.length = 0) || List.length queue.outlist > 0
    end
*)

let empty = { inlist = [];
              outlist = [];
              length = 0;
            }

(*****************************************)

let push el queue =
  if queue.outlist = [] then
    let outlist = List.rev (el::queue.inlist)
    in { inlist = [];
         outlist = outlist;
         length = queue.length + 1;
       }
  else
    { inlist = el::queue.inlist;
      outlist = queue.outlist;
      length = queue.length + 1;
    }

let enq = push
(*****************************************)

let top queue =
  match queue.outlist with
      [] -> (if queue.inlist != []
             then failwith "FQueue.top: BUG. inlist should be empty but isn't"
             else raise Empty)
    | hd::tl -> hd

(*****************************************)

let pop queue = match queue.outlist with
    hd::[] -> (hd, { inlist = [];
                     outlist = (List.rev queue.inlist);
                     length = queue.length - 1})
  | hd::tl -> (hd, { inlist = queue.inlist;
                     outlist = tl;
                     length = queue.length - 1;})
  | [] ->
      if queue.inlist = []
      then raise Empty
      else (match List.rev queue.inlist with
                [] -> failwith "FQueue.top: BUG.  inlist should not be empty here"
              | hd::tl -> (hd, { inlist=[];
                                 outlist=tl;
                                 length = queue.length - 1;
                               }))

(*****************************************)

let discard queue =
  let (el,new_q) = pop queue in
    new_q

let deq = pop

(*****************************************)

let to_list queue =
  queue.inlist @ (List.rev (queue.outlist))

(*****************************************)

let length queue = queue.length

let is_empty queue = queue.length = 0
