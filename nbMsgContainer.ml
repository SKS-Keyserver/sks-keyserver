(***********************************************************************)
(* nbMsgContainer.ml - message wrapper that allows for non-blocking    *)
(*                     reads.  Warning: this should be used only with  *)
(*                     one channel, since it keeps track of the last   *)
(*                     size read.                                      *)
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

open Common
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

  let bufc = Channel.new_buffer_outc 512

  type msg_container =
      { msg: Msg.msg_t;
        (* nonce: int; *)
      }

  let marshal_noflush cout msg =
    Buffer.clear bufc#buffer_nocopy;
    Msg.print (sprintf "Marshalling: %s" (Msg.to_string msg));
    Msg.marshal bufc#upcast msg;
    cout#write_int (Buffer.length bufc#buffer_nocopy);
    Buffer.output_buffer cout#outchan bufc#buffer_nocopy

  let marshal cout msg =
    marshal_noflush cout msg;
    cout#flush

  let last_length = (ref None : int option ref)

  (** Do a non-blocking message read *)
  let try_unmarshal cin =
    let oldalarm = Unix.alarm 0 in
    Unix.set_nonblock cin#fd;
    let run () =
      try
        let length = match !last_length with
          | Some x -> x
          | None ->
              let x = cin#read_int in
              last_length := Some x;
              x
        in
        let msgstr = cin#read_string length in
        last_length := None;
        let sin = new Channel.string_in_channel msgstr 0 in
        let msg = Msg.unmarshal sin#upcast
        in
        Msg.print (sprintf "Unmarshalling: %s (NB)" (Msg.to_string msg));
        Some { msg = msg; }
      with
        | Unix.Unix_error (Unix.EAGAIN,_,_)
        | Unix.Unix_error (Unix.EWOULDBLOCK,_,_)
        | Sys_blocked_io ->
            Msg.print "Operation would have blocked";
            None
    in
    protect ~f:run ~finally:(fun () ->
                               Unix.clear_nonblock cin#fd;
                               ignore (Unix.alarm oldalarm);
                            )

  (** Do a blocking message read *)
  let unmarshal cin =
    (* skip over the length, since we only need it in the nonblocking case *)
    let length = match !last_length with
      | Some x -> x
      | None -> cin#read_int
    in
    last_length := None;
    let msgstr = cin#read_string length in
    let sin = new Channel.string_in_channel msgstr 0 in
    let msg = Msg.unmarshal sin#upcast in
    Msg.print (sprintf "Unmarshalling: %s" (Msg.to_string msg));
    { msg = msg; }

end


