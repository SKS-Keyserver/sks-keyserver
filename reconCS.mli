val connect :
  'a PrefixTree.tree ->
  filters:string list ->
  partner:UnixLabels.addr_info -> ZZp.Set.t * UnixLabels.sockaddr
val handle_connection :
  'a PrefixTree.tree ->
  filters:string list ->
  partner:UnixLabels.sockaddr ->
  < fd : UnixLabels.file_descr; read_int : int;
    read_string : int -> string; .. > ->
  < flush : 'b; outchan : out_channel; upcast : #Channel.out_channel_obj;
    write_byte : int -> unit; write_char : char -> unit;
    write_float : float -> unit; write_int : int -> unit;
    write_int32 : int32 -> unit; write_int64 : int64 -> unit;
    write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  ZZp.Set.t * UnixLabels.sockaddr
