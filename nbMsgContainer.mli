module type MsgMarshal =
  sig
    type msg_t
    val marshal : Channel.out_channel_obj -> msg_t -> unit
    val unmarshal : Channel.in_channel_obj -> msg_t
    val to_string : msg_t -> string
    val print : string -> unit
  end
module Container :
  functor (Msg : MsgMarshal) ->
    sig
      val bufc : Channel.buffer_out_channel
      type msg_container = { msg : Msg.msg_t; }
      val marshal_noflush :
        < outchan : out_channel; write_int : int -> 'a; .. > ->
        Msg.msg_t -> unit
      val marshal :
        < flush : 'a; outchan : out_channel; write_int : int -> 'b; .. > ->
        Msg.msg_t -> 'a
      val last_length : int option ref
      val try_unmarshal :
        < fd : Unix.file_descr; read_int : int; read_string : int -> string;
          .. > ->
        msg_container option
      val unmarshal :
        < read_int : int; read_string : int -> string; .. > -> msg_container
    end
