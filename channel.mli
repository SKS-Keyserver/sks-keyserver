(***********************************************************************)
(* channel.mli - A generic, object-based channel interface for binary  *)
(*               input/output                                          *)
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

class virtual out_channel_obj :
  object
    method upcast : out_channel_obj
    method virtual write_byte : int -> unit
    method virtual write_char : char -> unit
    method write_float : float -> unit
    method write_int : int -> unit
    method write_int32 : int32 -> unit
    method write_int64 : int64 -> unit
    method virtual write_string : string -> unit
    method virtual write_string_pos :
      buf:string -> pos:int -> len:int -> unit
  end

class virtual in_channel_obj :
  object
    method virtual read_byte : int
    method virtual read_char : char
    method read_float : float
    method read_int : int
    method read_int32 : int32
    method read_int64 : int64
    method read_int64_size : int -> int64
    method read_int_size : int -> int
    method virtual read_string : int -> string
    method virtual read_string_pos : buf:string -> pos:int -> len:int -> unit
    method upcast : in_channel_obj
  end

(******************************************************************)

class sys_out_channel :
  out_channel ->
  object
    method close : unit
    method fd : Unix.file_descr
    method flush : unit
    method outchan : out_channel
    method skip : int -> unit
    method upcast : out_channel_obj
    method write_buf : Buffer.t -> unit
    method write_byte : int -> unit
    method write_char : char -> unit
    method write_float : float -> unit
    method write_int : int -> unit
    method write_int32 : int32 -> unit
    method write_int64 : int64 -> unit
    method write_string : string -> unit
    method write_string_pos : buf:string -> pos:int -> len:int -> unit
  end

class sys_in_channel :
  in_channel ->
  object
    method close : unit
    method fd : Unix.file_descr
    method inchan : in_channel
    method read_all : string
    method read_byte : int
    method read_char : char
    method read_float : float
    method read_int : int
    method read_int32 : int32
    method read_int64 : int64
    method read_int64_size : int -> int64
    method read_int_size : int -> int
    method read_string : int -> string
    method read_string_pos : buf:string -> pos:int -> len:int -> unit
    method upcast : in_channel_obj
  end

class buffer_out_channel :
  Buffer.t ->
  object
    method buffer_nocopy : Buffer.t
    method contents : string
    method upcast : out_channel_obj
    method write_byte : int -> unit
    method write_char : char -> unit
    method write_float : float -> unit
    method write_int : int -> unit
    method write_int32 : int32 -> unit
    method write_int64 : int64 -> unit
    method write_string : string -> unit
    method write_string_pos : buf:string -> pos:int -> len:int -> unit
  end

class string_in_channel :
  string ->
  int ->
  object
    method read_byte : int
    method read_char : char
    method read_float : float
    method read_int : int
    method read_int32 : int32
    method read_int64 : int64
    method read_int64_size : int -> int64
    method read_int_size : int -> int
    method read_string : int -> string
    method read_string_pos : buf:string -> pos:int -> len:int -> unit
    method read_rest : string
    method skip : int -> unit
    method upcast : in_channel_obj
    val mutable pos : int
  end

(*******************************************************************)

val new_buffer_outc : int -> buffer_out_channel
val sys_out_from_fd : Unix.file_descr -> sys_out_channel
val sys_in_from_fd : Unix.file_descr -> sys_in_channel

(*
class nonblocking_reader :
  Unix.file_descr ->
  object
    method read : string_in_channel option
  end

class nonblocking_writer :
  Unix.file_descr ->
  object
    method set_data : string -> unit
    method write : bool
  end
*)
