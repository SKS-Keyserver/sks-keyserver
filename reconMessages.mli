module ZSet :
  sig
    type elt = ZZp.zz
    type t = ZZp.Set.t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : f:(elt -> unit) -> t -> unit
    val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
    val for_all : f:(elt -> bool) -> t -> bool
    val exists : f:(elt -> bool) -> t -> bool
    val filter : f:(elt -> bool) -> t -> t
    val partition : f:(elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
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
  f:((< write_int : int -> 'b; .. > as 'a) -> ZSet.elt -> unit) ->
  'a -> ZSet.t -> unit
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

module Map :
  sig
    type ('a, 'b) t = ('a, 'b) PMap.Map.t
    val empty : ('a, 'b) t
    val add : key:'a -> data:'b -> ('a, 'b) t -> ('a, 'b) t
    val find : 'a -> ('a, 'b) t -> 'b
    val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
    val mem : 'a -> ('a, 'b) t -> bool
    val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
    val map : f:('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
    val mapi : f:(key:'a -> data:'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    val fold :
      f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
    val of_alist : ('a * 'b) list -> ('a, 'b) t
    val to_alist : ('a, 'b) t -> ('a * 'b) list
  end
val marshal_ZZp : < write_string : string -> 'a; .. > -> ZZp.zz -> 'a
val unmarshal_ZZp : < read_string : int -> string; .. > -> ZZp.zz
val marshal_zzarray :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  ZZp.mut_array -> unit
val unmarshal_zzarray :
  < read_int : int; read_string : int -> string; .. > -> ZZp.mut_array
val marshal_zset :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  ZSet.t -> unit
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
type recon_rqst_full = { rf_prefix : Bitstring.t; rf_elements : ZSet.t; }
val marshal_recon_rqst_full :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  recon_rqst_full -> unit
val unmarshal_recon_rqst_full :
  < read_int : int; read_string : int -> string; .. > -> recon_rqst_full
type configdata = (string, string) Map.t
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
  (string, string) Map.t -> unit
val unmarshal_configdata :
  < read_int : int; read_string : int -> 'a; .. > -> ('a, 'a) Map.t
val sockaddr_to_string : Unix.sockaddr -> string
val marshal_allreply :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Bitstring.t * ZSet.t -> unit
val unmarshal_allreply :
  < read_int : int; read_string : int -> string; .. > ->
  Bitstring.t * ZZp.Set.t
type msg =
    ReconRqst_Poly of recon_rqst_poly
  | ReconRqst_Full of recon_rqst_full
  | Elements of ZSet.t
  | FullElements of ZSet.t
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
