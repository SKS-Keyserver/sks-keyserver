exception Bug of string
val pos_next_rec :
  ('a * Packet.packet) SStream.sstream ->
  Packet.packet list -> Packet.packet list option
val pos_next :
  ('a * Packet.packet) SStream.sstream -> ('a * Packet.packet list) option
val pos_get : ('a * Packet.packet) SStream.sstream -> 'a * Packet.packet list
val pos_next_of_channel :
  < inchan : in_channel; read_byte : int; read_string : int -> string; .. > ->
  unit -> (int64 * Packet.packet list) option
val pos_get_of_channel :
  < inchan : in_channel; read_byte : int; read_string : int -> string; .. > ->
  unit -> int64 * Packet.packet list
val next_rec :
  Packet.packet SStream.sstream ->
  Packet.packet list -> Packet.packet list option
val next : Packet.packet SStream.sstream -> Packet.packet list option
val get : Packet.packet SStream.sstream -> Packet.packet list
val next_of_channel :
  < read_byte : int; read_string : int -> string; .. > ->
  unit -> Packet.packet list option
val get_of_channel :
  < read_byte : int; read_string : int -> string; .. > ->
  unit -> Packet.packet list
val get_ids : Packet.packet list -> string list
val write :
  Packet.packet list ->
  < write_byte : int -> 'a; write_int : int -> 'b;
    write_string : string -> unit; .. > ->
  unit
val to_string : Packet.packet list -> string
val of_string : string -> Packet.packet list
val of_string_multiple : string -> Packet.packet list list
val to_string_multiple : Packet.packet list list -> string
val to_words : Packet.packet list -> string list
