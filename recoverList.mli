type recover_element = string list * UnixLabels.sockaddr
val hash_bundle_size : int
val recover_list : recover_element Queue.t
val gossip_disabled_var : bool ref
val gossip_disabled : unit -> bool
val disable_gossip : unit -> unit
val enable_gossip : unit -> unit
val n_split : 'a list -> int -> 'a list * 'a list
val size_split : 'a list -> int -> 'a list list
val print_hashes : string -> string list -> unit
val hashconvert : ZZp.zz list -> string list
val log_diffs : string -> string list -> unit
val update_recover_list : ZZp.zz list -> UnixLabels.sockaddr -> unit
