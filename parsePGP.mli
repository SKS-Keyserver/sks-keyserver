exception Overlong_mpi
exception Partial_body_length of int
val parse_new_packet_length : < read_byte : int; .. > -> int
val read_packet :
  < read_byte : int; read_string : int -> string; .. > -> Packet.packet
val offset_read_packet :
  < inchan : in_channel; read_byte : int; read_string : int -> string; .. > ->
  int64 * Packet.packet
val offset_length_read_packet :
  < inchan : in_channel; read_byte : int; read_string : int -> string; .. > ->
  Packet.packet * int * int
val read_mpi :
  < read_byte : int; read_string : int -> string; .. > -> Packet.mpi
val read_mpis :
  < read_byte : int; read_string : int -> string; .. > -> Packet.mpi list
val parse_pubkey_info : Packet.packet -> Packet.pubkeyinfo
val parse_sigsubpacket_length : < read_byte : int; .. > -> int
val read_sigsubpacket :
  < read_byte : int; read_string : int -> string; .. > -> Packet.sigsubpacket
val get_hashed_subpacket_string :
  < read_byte : int; read_int_size : int -> 'a; read_string : 'a -> 'b; .. > ->
  'b
val read_subpackets :
  < read_string : 'a -> string; .. > -> 'a -> Packet.sigsubpacket list
val parse_signature : Packet.packet -> Packet.signature
val ssp_ctime_id : int
val ssp_exptime_id : int
val int32_of_string : string -> int32
val int64_of_string : string -> int64
val get_times : Packet.signature -> int64 option * int64 option
val get_key_exptimes : Packet.signature -> int64 option * int64 option
