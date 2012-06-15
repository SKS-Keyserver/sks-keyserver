module Set :
  sig
    type 'a t = 'a PSet.Set.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val singleton : 'a -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    val subset : 'a t -> 'a t -> bool
    val iter : f:('a -> unit) -> 'a t -> unit
    val fold : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val partition : f:('a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val elements : 'a t -> 'a list
    val min_elt : 'a t -> 'a
    val max_elt : 'a t -> 'a
    val choose : 'a t -> 'a
    val of_list : 'a list -> 'a t
  end
type configvar =
    [ `float of float | `int of int | `none | `string of string ]
val marshal_config :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string * [< `float of float | `int of int | `none | `string of string ] ->
  unit
val unmarshal_config :
  < read_byte : int; read_float : 'a; read_int : 'b; read_string : 'b -> 'c;
    .. > ->
  'c * [> `float of 'a | `int of 'b | `none | `string of 'c ]
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
val marshal_timestamp : < write_float : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_timestamp : < read_float : 'a; .. > -> 'a
val marshal_logquery :
  < write_float : 'a -> 'b; write_int : 'c -> 'd; .. > -> 'c * 'a -> 'b
val unmarshal_logquery : < read_float : 'a; read_int : 'b; .. > -> 'b * 'a
val marshal_event :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Common.event -> unit
val unmarshal_event :
  < read_byte : int; read_int : 'a; read_string : 'a -> string; .. > ->
  Common.event
val marshal_log_entry :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  float * Common.event -> unit
val unmarshal_log_entry :
  < read_byte : int; read_float : 'a; read_int : 'b;
    read_string : 'b -> string; .. > ->
  'a * Common.event
val marshal_key :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Packet.packet list -> unit
val unmarshal_key :
  < read_int : 'a; read_string : 'a -> string; .. > -> Packet.packet list
val marshal_key_list :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Packet.packet list list -> unit
val unmarshal_key_list :
  < read_int : int; read_string : int -> string; .. > ->
  Packet.packet list list
val marshal_missingkeys :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string list * Unix.sockaddr -> unit
val unmarshal_missingkeys :
  < read_byte : int; read_int : int; read_string : int -> string; .. > ->
  string list * Unix.sockaddr
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
module M :
  sig
    type msg_container = { msg : msg; }
    val marshal_noflush :
      < upcast : Channel.out_channel_obj; .. > -> msg -> unit
    val marshal :
      < flush : 'a; upcast : Channel.out_channel_obj; .. > -> msg -> 'a
    val unmarshal : < upcast : Channel.in_channel_obj; .. > -> msg_container
  end
type msg_container = M.msg_container = { msg : msg; }
val marshal_noflush : < upcast : Channel.out_channel_obj; .. > -> msg -> unit
val marshal :
  < flush : 'a; upcast : Channel.out_channel_obj; .. > -> msg -> 'a
val unmarshal : < upcast : Channel.in_channel_obj; .. > -> msg_container
