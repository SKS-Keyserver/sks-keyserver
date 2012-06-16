val marshal_string :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string -> unit
val unmarshal_string : < read_int : 'a; read_string : 'a -> 'b; .. > -> 'b
val marshal_lstring : < write_string : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_lstring : 'a -> < read_string : 'a -> 'b; .. > -> 'b
val marshal_array :
  f:((< write_int : int -> 'b; .. > as 'a) -> 'c -> unit) ->
  'a -> 'c array -> unit
val unmarshal_array :
  f:((< read_int : int; .. > as 'a) -> 'b) -> 'a -> 'b array
val marshal_list :
  f:((< write_int : int -> 'b; .. > as 'a) -> 'c -> unit) ->
  'a -> 'c list -> unit
val unmarshal_list :
  f:((< read_int : int; .. > as 'a) -> 'b) -> 'a -> 'b list
val marshal_fixed_sarray :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  string array -> unit
val unmarshal_fixed_sarray :
  < read_int : int; read_string : int -> 'a; .. > -> 'b -> 'a array
val marshal_bitstring :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Bitstring.t -> unit
val unmarshal_bitstring :
  < read_int : int; read_string : int -> string; .. > -> Bitstring.t
val marshal_set :
  f:((< write_int : int -> 'b; .. > as 'a) -> ZZp.zz -> unit) ->
  'a -> ZZp.Set.t -> unit
val unmarshal_set :
  f:((< read_int : int; .. > as 'a) -> ZZp.zz) -> 'a -> ZZp.Set.t
val marshal_sockaddr :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Unix.sockaddr -> unit
val unmarshal_sockaddr :
  < read_byte : int; read_int : int; read_string : int -> string; .. > ->
  Unix.sockaddr
val marshal_to_string :
  f:(Channel.buffer_out_channel -> 'a -> 'b) -> 'a -> string
val unmarshal_from_string :
  f:(Channel.string_in_channel -> 'a) -> string -> 'a
val int_to_string : int -> string
val int_of_string : string -> int

val marshal_ZZp : < write_string : string -> 'a; .. > -> ZZp.zz -> 'a
val unmarshal_ZZp : < read_string : int -> string; .. > -> ZZp.zz
val marshal_zzarray :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  ZZp.mut_array -> unit
val unmarshal_zzarray :
  < read_int : int; read_string : int -> string; .. > -> ZZp.mut_array
val marshal_zset :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  ZZp.Set.t -> unit
val unmarshal_zset :
  < read_int : int; read_string : int -> string; .. > -> ZZp.Set.t
type recon_rqst_poly = {
  rp_prefix : Bitstring.t;
  rp_size : int;
  rp_samples : ZZp.mut_array;
}
val marshal_recon_rqst_poly :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  recon_rqst_poly -> unit
val unmarshal_recon_rqst_poly :
  < read_int : int; read_string : int -> string; .. > -> recon_rqst_poly
type recon_rqst_full = { rf_prefix : Bitstring.t; rf_elements : ZZp.Set.t; }
val marshal_recon_rqst_full :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  recon_rqst_full -> unit
val unmarshal_recon_rqst_full :
  < read_int : int; read_string : int -> string; .. > -> recon_rqst_full
type configdata = (string, string) PMap.Map.t
val marshal_stringpair :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string * string -> unit
val unmarshal_stringpair :
  < read_int : 'a; read_string : 'a -> 'b; .. > -> 'b * 'b
val marshal_stringpair_list :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  (string * string) list -> unit
val unmarshal_stringpair_list :
  < read_int : int; read_string : int -> 'a; .. > -> ('a * 'a) list
val marshal_configdata :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  (string, string) PMap.Map.t -> unit
val unmarshal_configdata :
  < read_int : int; read_string : int -> 'a; .. > -> ('a, 'a) PMap.Map.t
val sockaddr_to_string : Unix.sockaddr -> string
val marshal_allreply :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Bitstring.t * ZZp.Set.t -> unit
val unmarshal_allreply :
  < read_int : int; read_string : int -> string; .. > ->
  Bitstring.t * ZZp.Set.t
type msg =
  | ReconRqst_Poly of recon_rqst_poly
  | ReconRqst_Full of recon_rqst_full
  | Elements of ZZp.Set.t
  | FullElements of ZZp.Set.t
  | SyncFail
  | Done
  | Flush
  | Error of string
  | DbRqst of string
  | DbRepl of string
  | Config of configdata
val msg_to_string : msg -> string
val print_msg : msg -> unit
val marshal_samplevalues :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  int * string array -> unit
val unmarshal_samplevalues :
  < read_int : int; read_string : int -> 'a; .. > -> int * ('b -> 'a array)
val marshal_time : float ref
val unmarshal_time : float ref
val timer : MTimer.t
val marshal_msg :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  msg -> unit
val unmarshal_msg :
  < read_byte : int; read_int : int; read_string : int -> string; .. > -> msg
module M :
  sig
    val bufc : Channel.buffer_out_channel
    type msg_container = { msg : msg; }
    val marshal_noflush :
      < outchan : out_channel; write_int : int -> 'a; .. > -> msg -> unit
    val marshal :
      < flush : 'a; outchan : out_channel; write_int : int -> 'b; .. > ->
      msg -> 'a
    val last_length : int option ref
    val try_unmarshal :
      < fd : NbMsgContainer.Unix.file_descr; read_int : int;
        read_string : int -> string; .. > ->
      msg_container option
    val unmarshal :
      < read_int : int; read_string : int -> string; .. > -> msg_container
  end
val bufc : Channel.buffer_out_channel
type msg_container = M.msg_container = { msg : msg; }
val marshal_noflush :
  < outchan : out_channel; write_int : int -> 'a; .. > -> msg -> unit
val marshal :
  < flush : 'a; outchan : out_channel; write_int : int -> 'b; .. > ->
  msg -> 'a
val last_length : int option ref
val try_unmarshal :
  < fd : NbMsgContainer.Unix.file_descr; read_int : int;
    read_string : int -> string; .. > ->
  msg_container option
val unmarshal :
  < read_int : int; read_string : int -> string; .. > -> msg_container
