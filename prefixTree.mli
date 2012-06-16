type 'a tree
type node
type 'a db
type 'a disk

val create :
  ?db:(string -> string) * ('a option -> key:string -> data:string -> unit) *
      ('a option -> string -> unit) *
      ((unit -> 'a option) * ('a option -> unit) * ('a option -> unit)) *
      int ->
  txn:'a option ->
  num_samples:int -> bitquantum:int -> thresh:int -> unit -> 'a tree
val child_keys : 'a tree -> Bitstring.t -> Bitstring.t list
val get_zzp_elements : 'a tree -> node -> ZZp.Set.t
val clean : 'a option -> 'a tree -> unit
val points : 'a tree -> ZZp.zz array
val get_node_key : ?sef:bool -> 'a tree -> Bitstring.t -> node
val svalues : node -> ZZp.mut_array
val size : node -> int
val is_leaf : node -> bool
val num_elements : 'a -> node -> int
val elements : 'a tree -> node -> ZZp.Set.t
val root : 'a tree -> node
val get_random : 'a tree -> node -> string

val set_synctime : 'a tree -> float -> unit
val get_synctime : 'a tree -> float

val insert_str : 'a tree -> 'a option -> string -> unit
val delete_str : 'a tree -> 'a option -> string -> unit

val set_maxnodes : 'a tree -> 'a option -> int -> unit
