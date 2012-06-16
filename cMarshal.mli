val marshal_string :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string -> unit
val unmarshal_string : < read_int : 'a; read_string : 'a -> 'b; .. > -> 'b
val marshal_list :
  f:((< write_int : int -> 'b; .. > as 'a) -> 'c -> unit) ->
  'a -> 'c list -> unit
val unmarshal_list :
  f:((< read_int : int; .. > as 'a) -> 'b) -> 'a -> 'b list
val marshal_lstring : < write_string : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_lstring : 'a -> < read_string : 'a -> 'b; .. > -> 'b
val marshal_array :
  f:((< write_int : int -> 'b; .. > as 'a) -> 'c -> unit) ->
  'a -> 'c array -> unit
val unmarshal_array :
  f:((< read_int : int; .. > as 'a) -> 'b) -> 'a -> 'b array
val marshal_bitstring :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Bitstring.t -> unit
val unmarshal_bitstring :
  < read_int : int; read_string : int -> string; .. > -> Bitstring.t
val marshal_fixed_sarray :
  < write_int : int -> 'a; write_string : string -> unit; .. > ->
  string array -> unit
val unmarshal_fixed_sarray :
  < read_int : int; read_string : int -> 'a; .. > -> 'b -> 'a array
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
