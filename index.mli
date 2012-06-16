type siginfo
val empty_siginfo : unit -> siginfo
val keyinfo_header : Request.request -> string
val sig_to_siginfo : Packet.packet -> siginfo
val sort_siginfo_list : siginfo list -> siginfo list
val is_selfsig : keyid:string -> siginfo -> bool
val is_primary : keyid:string -> Packet.packet * siginfo list -> bool
val max_selfsig_time : keyid:string -> 'a * siginfo list -> float
val split_list : f:('a -> bool) -> 'a list -> 'a list * 'a list
val move_primary_to_front :
  keyid:string ->
  (Packet.packet * siginfo list) list -> (Packet.packet * siginfo list) list
val convert_sigpair : 'a * Packet.packet list -> 'a * siginfo list
val blank_datestr : string
val no_datestr : string
val datestr_of_int64 : int64 -> string
val siginfo_to_lines :
  get_uid:(string -> string option) ->
  ?key_creation_time:int64 ->
  Request.request -> string -> float -> siginfo -> string list
val selfsigs_to_lines :
  Request.request ->
  int64 -> string -> Packet.packet list -> float -> string list
val uid_to_lines :
  get_uid:(string -> string option) ->
  Request.request ->
  int64 -> string -> float -> Packet.packet * siginfo list -> string list
val uids_to_lines :
  get_uid:(string -> string option) ->
  Request.request ->
  int64 ->
  string -> (Packet.packet * siginfo list) list -> float -> string list
val key_packet_to_line :
  is_subkey:bool -> Packet.pubkeyinfo -> string -> string * string
val subkey_to_lines :
  Request.request -> float -> Packet.packet * siginfo list -> string list
val subkeys_to_lines :
  Request.request ->
  (Packet.packet * siginfo list) list -> float -> string list
val extract : f:('a -> bool) -> 'a list -> 'a option * 'a list
val move_to_front : f:('a -> bool) -> 'a list -> 'a list
val get_uid :
  (string -> (Packet.packet * Packet.packet list) list) ->
  string -> string option
val get_extra_lines :
  Request.request -> 'a -> string -> Fingerprint.result -> string list
val key_to_lines_verbose :
  get_uids:(string -> (Packet.packet * Packet.packet list) list) ->
  Request.request -> Packet.packet list -> string -> string list
val sig_is_revok : siginfo -> bool
val is_revoked : Packet.packet list -> bool
val key_to_lines_normal :
  Request.request -> Packet.packet list -> string -> string list
