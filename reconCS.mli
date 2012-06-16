val build_configdata : string list -> (string, string) PMap.Map.t
val comma_rxp : Str.regexp
val config_get_filters : (string, string) PMap.Map.t -> string list
val test_configdata :
  (string, string) PMap.Map.t ->
  (string, string) PMap.Map.t -> [> `failed of string | `passed ]
val handle_config :
  < read_int : int; read_string : int -> string; .. > ->
  < flush : 'a; outchan : out_channel; upcast : #Channel.out_channel_obj;
    write_byte : int -> unit; write_char : char -> unit;
    write_float : float -> unit; write_int : int -> unit;
    write_int32 : int32 -> unit; write_int64 : int64 -> unit;
    write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string list -> UnixLabels.sockaddr -> ReconMessages.configdata
val config_get_http_port : (string, string) PMap.Map.t -> int
val change_port : UnixLabels.sockaddr -> int -> UnixLabels.sockaddr
val print_config : (string, string) PMap.Map.t -> unit
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
