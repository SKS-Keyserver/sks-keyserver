exception Bad_key
exception Standalone_revocation_certificate
val filters : string list
val get_keypacket : KeyMerge.pkey -> Packet.packet
val ( |= ) : ('a, 'b) PMap.Map.t -> 'a -> 'b
val ( |< ) : ('a, 'b) PMap.Map.t -> 'a * 'b -> ('a, 'b) PMap.Map.t
val join_by_keypacket : KeyMerge.pkey list -> KeyMerge.pkey list list
val merge_pkeys : KeyMerge.pkey list -> KeyMerge.pkey
val compute_merge_replacements :
  Packet.packet list list ->
  (Packet.packet list list * Packet.packet list) list
val canonicalize : Packet.packet list -> Packet.packet list
val good_key : Packet.packet -> bool
val good_signature : Packet.packet -> bool
val drop_bad_sigs : Packet.packet list -> Packet.packet list
val sig_filter_sigpair :
  'a * Packet.packet list -> ('a * Packet.packet list) option
val presentation_filter : Packet.packet list -> Packet.packet list option
