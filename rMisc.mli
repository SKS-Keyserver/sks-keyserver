val det_rng : Random.State.t
val stringset_to_string : string PSet.Set.t -> string
val digest_stringset : string PSet.Set.t -> Digest.t
val print_lengths : string list -> unit
val fill_random_string :
  (unit -> int) -> string -> pos:int -> len:int -> unit
val random_string : (unit -> int) -> int -> string
val conv_chans :
  in_channel * out_channel ->
  MeteredChannel.metered_in_channel * MeteredChannel.metered_out_channel
val add_random : (unit -> int) -> int -> string PSet.Set.t -> string PSet.Set.t
val add_n_random :
  (unit -> int) -> int -> n:int -> string PSet.Set.t -> string PSet.Set.t
val det_string_set : bytes:int -> size:int -> string PSet.Set.t
val rand_string_set : bytes:int -> size:int -> string PSet.Set.t
val localize_string_set :
  bytes:int -> diff:int -> string PSet.Set.t -> string PSet.Set.t
val add_sarray : data:'a PSet.Set.t -> 'a array -> 'a PSet.Set.t
val pad : string -> int -> string
val padset : string PSet.Set.t -> int -> string PSet.Set.t
val truncate : string -> int -> string
val truncset : string PSet.Set.t -> int -> string PSet.Set.t
val order_string : string
val print_ZZp_list : ZZp.zz list -> unit
val print_ZZp_set : ZZp.zz PSet.Set.t -> unit
