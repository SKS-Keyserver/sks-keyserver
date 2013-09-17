(***********************************************************************)
(* sStream.ml - simple stream with 1-step lookahead.                   *)
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

type 'b sstream = { mutable first: 'b option;
                    next: unit -> 'b option;
                  }

let make ?first next = { first = first;
                         next = next;
                       }

let next s =
  match s.first with
      None -> s.next ()
    | v ->
        s.first <- None;
        v

let peek s =
  if s.first = None
  then s.first <- s.next ();
  s.first

let junk s =
  if s.first = None
  then ignore (s.next ())
  else s.first <- None



