type recon_rqst_poly = {
  rp_prefix : Bitstring.t;
  rp_size : int;
  rp_samples : ZZp.mut_array;
}
type recon_rqst_full = { rf_prefix : Bitstring.t; rf_elements : ZZp.Set.t; }
type configdata = (string, string) PMap.Map.t
val sockaddr_to_string : Unix.sockaddr -> string
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
