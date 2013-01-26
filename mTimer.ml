(***********************************************************************)
(* mTimer.ml - Simple timer module                                     *)
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

type t = { mutable start_time : float;
           mutable stop_time : float;
           mutable running : bool;
         }

let create () = { start_time = 0.0;
                  stop_time = 0.0;
                  running = false;
                }

let reset timer =
  timer.start_time <- 0.0;
  timer.stop_time <- 0.0;
  timer.running <- false

let start timer =
  ( timer.start_time <- Unix.gettimeofday ();
    timer.running <- true )

let stop timer =
  if not timer.running then failwith "Timer stopped when not running."
  else ( timer.stop_time <- Unix.gettimeofday ();
         timer.running <- false )

let read timer =
  if timer.running
  then failwith "Timer read at wrong time"
  else timer.stop_time -. timer.start_time

let read_ms timer = 1000.0 *. (read timer)
let read_us timer = (1000.0 *. 1000.0) *. (read timer)

