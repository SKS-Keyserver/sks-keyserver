external crc_of_string : string -> int = "caml_crc_octets"
val base64crc : string -> string
val pubkey_armor_header : string
val pubkey_armor_tail : string
val encode_pubkey : Packet.packet list -> string
val encode_pubkey_string : string -> string
val decode_crc : string -> int
val eol : Str.regexp
val decode_pubkey : string -> Packet.packet list list
