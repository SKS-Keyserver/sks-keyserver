type configvar =
    [ `float of float | `int of int | `none | `string of string ]

type msg =
    WordQuery of string list
  | LogQuery of (int * Common.timestamp)
  | HashRequest of string list
  | LogResp of (Common.timestamp * Common.event) list
  | Keys of Packet.key list
  | KeyStrings of string list
  | Ack of int
  | MissingKeys of (string list * Unix.sockaddr)
  | Synchronize
  | RandomDrop of int
  | ProtocolError
  | DeleteKey of string
  | Config of (string * configvar)
  | Filters of string list

val marshal_msg :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : Common.timestamp -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  msg -> unit
val unmarshal_msg :
  < read_byte : int; read_float : Common.timestamp; read_int : int;
    read_string : int -> string; .. > ->
  msg
val sockaddr_to_string : Unix.sockaddr -> string
val msg_to_string : msg -> string
module M : sig
  type msg_container = { msg : msg; }
end
type msg_container = M.msg_container = { msg : msg; }
val marshal_noflush : < upcast : Channel.out_channel_obj; .. > -> msg -> unit
val marshal :
  < flush : 'a; upcast : Channel.out_channel_obj; .. > -> msg -> 'a
val unmarshal : < upcast : Channel.in_channel_obj; .. > -> msg_container
