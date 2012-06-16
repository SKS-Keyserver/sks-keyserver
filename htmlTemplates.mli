val html_quote : string -> string
val page : title:string -> body:string -> string
val link :
  op:string -> hash:bool -> fingerprint:bool -> keyid:string -> string
val keyinfo_header : string
val keyinfo_pks :
  Packet.pubkeyinfo ->
  bool -> keyid:string -> link:string -> userids:string list -> string
val fingerprint : fp:string -> string
val hash_link : hash:string -> string
val hash : hash:string -> string
val preformat_list : string list -> string