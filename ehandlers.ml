(***********************************************************************)
(* ehandlers.ml - functions for constructing event handlers for use    *)
(*                with [Eventloop] module                              *)
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

open Common
open Eventloop
module Unix = UnixLabels

(** Repeat callback ~request with a gap of redo_timeout, until
  either (test ()) is true or full_timeout has expired.
  In the former case, invoke success, int the latter, failure.

  Callbacks can return a list of events, which will be placed
  on the queue upon their completion.
 *)
let repeat_until ~redo_timeout ~full_timeout ~test
  ~init ~request ~success ~failure =
  init ();
  let start = Unix.gettimeofday () in
  let rec loop () =
    let now = Unix.gettimeofday () in
    if test ()
    then success ()
    else if now > start +. full_timeout
    then failure ()
    else
      (request ()) @
      [ Event (now +. redo_timeout, Callback loop) ]
  in
  let now = Unix.gettimeofday () in
  [ Event  (now, Callback loop) ]


(** returns smallest floating point number larger than the argument *)
let float_incr x = x +. x *. epsilon_float
let float_decr x = x -. x *. epsilon_float

let strftime time =
  let tm = Unix.localtime time in
  sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec


(** repeat provided callback forever, with one invocation occuring timeout
  seconds after the last one completed. *)
let repeat_forever ?(jitter=0.0) ?start timeout callback =
  let rec loop () =
    let delay = timeout +. (Random.float jitter -. jitter /. 2.) *. timeout in
    let next_time = Unix.gettimeofday () +. delay in
    [ Event (next_time, callback);
      Event (float_incr next_time, Callback loop);
    ]
  in
  let start = match start with
      None -> Unix.gettimeofday ()
    | Some time -> time
  in
  [ Event (start, Callback loop); ]


let repeat_forever_simple timeout callback =
  repeat_forever timeout (Callback (fun () -> callback (); []))


let incr_day time =
  let tm = Unix.localtime time in
  let tm = {tm with Unix.tm_mday = tm.Unix.tm_mday + 1; } in
  let (time,tm) = Unix.mktime tm in
  time

let set_hour time hour =
  let tm = Unix.localtime time in
  let tm = {tm with
              Unix.tm_sec = 0;
              Unix.tm_min = 0;
              Unix.tm_hour = hour;
              Unix.tm_mday = tm.Unix.tm_mday +
                             if hour < tm.Unix.tm_hour then 1 else 0
           }
  in
  let (time,tm) = Unix.mktime tm in
  time

let repeat_at_hour hour callback =
  let rec loop oldtime () =
    let newtime = incr_day oldtime in
    [ Event (oldtime, Callback callback);
      Event (newtime, Callback (loop newtime)) ]
  in
  let start = set_hour (Unix.gettimeofday ()) hour in
  [Event (start, Callback (loop start)) ]
