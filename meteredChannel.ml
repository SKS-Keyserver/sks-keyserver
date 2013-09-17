(***********************************************************************)
(* meteredChannel.ml - Version of the [Channel] objects that keeps     *)
(*                     track of the number of bytes sent through them. *)
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


class metered_out_channel outc =
object (self)
  inherit Channel.out_channel_obj

  val mutable count = 0

  method private incr c = count <- count + c

  method write_string str =
    outc#write_string str;
    self#incr (String.length str)

  method write_string_pos ~buf ~pos ~len =
    outc#write_string_pos ~buf ~pos ~len;
    self#incr len

  method write_char char =
    outc#write_char char;
    self#incr 1

  method write_byte byte =
    outc#write_byte byte;
    self#incr 1

  method flush : unit = outc#flush
  method upcast = (self :> Channel.out_channel_obj)
  method reset = count <- 0
  method bytes = count

end


class metered_in_channel inc =
object (self)
  inherit Channel.in_channel_obj

  val mutable count = 0

  method private incr c = count <- count + c

  method read_string len =
    self#incr len;
    inc#read_string len

  method read_string_pos ~buf ~pos ~len =
    self#incr len;
    inc#read_string_pos ~buf ~pos ~len

  method read_char =
    self#incr 1;
    inc#read_char

  method read_byte =
    self#incr 1;
    inc#read_byte

  method upcast = (self :> Channel.in_channel_obj)
  method reset = count <- 0
  method bytes = count

end
