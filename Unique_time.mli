(***********************************************************************)
(* Unique_time.mli - Module to return unique time                      *)
(*                 @author Yaron M. Minsky                             *)
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

(* An interface to Unix.gettimeofday() which enforces that time always goes
 * up and never repeats.
 * get() returns seconds & microseconds, so minimum meaningful
 * increment is 1 microsecond; OCaml uses IEEE 754 double-precision floats,
 * which gives 53 bits of mantissa.  Assuming 32 bits for time until 32-bit
 * time_t overflows, we can knock bits off 21 bits depending upon when we want
 * the overflow/rollover to occur, and whatever's left is available for delta
 * even at the end of the lifetime of the code; as that fateful day approaches,
 * lower the granularity of this delay accordly
 * we don't use epsilon_float, as that's only guaranteed to give a different
 * result when added to 1.0, not for other numbers.
 * If wallclock time goes backwards, we won't, but time will appear to go
 * forward very very slowly until wallclock catches back up
 *)

val get : 'a -> float
