(***********************************************************************)
(* msgContainer.ml                                                     *)
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

open Printf

module type MsgMarshal =
sig
  type msg_t
  val marshal: Channel.out_channel_obj -> msg_t -> unit
  val unmarshal: Channel.in_channel_obj -> msg_t
  val to_string: msg_t -> string
  val print: string -> unit
end

module Container =
  functor (Msg:MsgMarshal) ->
struct

  type msg_container =
      { msg: Msg.msg_t;
        (* nonce: int; *)
      }

  let marshal_noflush cout msg =
    Msg.print (sprintf "Marshalling: %s" (Msg.to_string msg));
    Msg.marshal cout#upcast msg

  let marshal cout msg =
    marshal_noflush cout msg;
    cout#flush

  let unmarshal cin =
    let msg = Msg.unmarshal cin#upcast in
    Msg.print (sprintf "Unmarshalling: %s" (Msg.to_string msg));
    { msg = msg; }

end


