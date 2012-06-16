module Set :
  sig
    type 'a t = 'a PSet.Set.t
  end
module ZSet :
  sig
    type t = ZZp.Set.t
  end
exception Bug of string
type key = Bitstring.t
module WHash :
  sig
    type data = key
    type t
    val create : int -> t
    val clear : t -> unit
    val merge : t -> data -> data
    val add : t -> data -> unit
    val remove : t -> data -> unit
    val find : t -> data -> data
    val find_all : t -> data -> data list
    val mem : t -> data -> bool
    val iter : (data -> unit) -> t -> unit
    val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
    val count : t -> int
    val stats : t -> int * int * int * int * int * int
  end
type writestatus = Clean | Dirty
type 'a disk = OnDisk of key | InMem of 'a
type children = Leaf of string Set.t | Children of node disk array
and node = {
  svalues : ZZp.mut_array;
  key : key;
  mutable num_elements : int;
  mutable children : children;
  mutable wstatus : writestatus;
}
type 'a db = {
  load : string -> string;
  save : 'a option -> key:string -> data:string -> unit;
  delete : 'a option -> string -> unit;
  create_txn : unit -> 'a option;
  commit_txn : 'a option -> unit;
  abort_txn : 'a option -> unit;
  mutable maxnodes : int;
  mutable inmem_count : int;
}
type 'a tree = {
  root : node;
  num_samples : int;
  split_thresh : int;
  join_thresh : int;
  bitquantum : int;
  points : ZZp.zz array;
  db : 'a db option;
  mutable synctime : float;
}
type dheader = {
  d_num_samples : int;
  d_split_thresh : int;
  d_join_thresh : int;
  d_bitquantum : int;
  d_points : ZZp.zz array;
}
val op_unwrap : 'a option -> 'a
val op_apply : f:('a -> 'b) -> 'a option -> 'b option
val op_map : f:('a -> 'b) -> 'a option list -> 'b option list
val child_keys_rec : Bitstring.t -> bit:int -> len:int -> Bitstring.t Set.t
val child_keys_raw : int -> Bitstring.t -> Bitstring.t list
val child_keys : 'a tree -> Bitstring.t -> Bitstring.t list
val marshal_to_string :
  f:(Channel.out_channel_obj -> 'a -> 'b) -> 'a -> string
val unmarshal_of_string : f:(Channel.in_channel_obj -> 'a) -> string -> 'a
val samesize : string Set.t -> bool
val marshal_node : Channel.out_channel_obj -> node -> unit
val unmarshal_node :
  bitquantum:int -> num_samples:int -> Channel.in_channel_obj -> node
val node_to_string : node -> string
val node_of_string_raw : bitquantum:int -> num_samples:int -> string -> node
val node_of_string : 'a tree -> string -> node
val marshal_header :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  'a tree -> unit
val unmarshal_dheader :
  < read_byte : int; read_char : char; read_float : float; read_int : 
    int; read_int32 : int32; read_int64 : int64;
    read_int64_size : int -> int64; read_int_size : int -> int;
    read_string : int -> string;
    read_string_pos : buf:string -> pos:int -> len:int -> unit;
    upcast : #Channel.in_channel_obj; .. > ->
  dheader
val header_to_string : 'a tree -> string
val dheader_of_string : string -> dheader
val dheader_to_header : 'a db option -> node -> dheader -> float -> 'a tree
val marshal_synctime : < write_float : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_synctime : < read_float : 'a; .. > -> 'a
val synctime_to_string : float -> string
val synctime_of_string : string -> float
val dbkey_of_key : Bitstring.t -> string
val int_to_bstring : int -> string
val root_dbkey : string
val header_dbkey : string
val synctime_dbkey : string
val load_node : 'a tree -> string -> node
val load_child : 'a tree -> node disk array -> int -> node
val load_child_sef : 'a tree -> node disk array -> int -> node
val save_node : 'a tree -> 'a option -> node -> unit
val save_synctime : 'a tree -> 'a option -> unit
val clean_subtree : 'a tree -> 'a option -> node -> unit
val clean : 'a option -> 'a tree -> unit
val delete_subtree_rec : 'a option -> 'a tree -> node disk -> unit
val delete_subtree : 'a option -> 'a tree -> node -> unit
val summarize_tree_rec :
  lagg:(string Set.t -> 'a) ->
  cagg:('a array -> 'a) -> 'b tree -> node disk -> 'a
val summarize_tree :
  lagg:(string Set.t -> 'a) -> cagg:('a array -> 'a) -> 'b tree -> 'a
val count_nodes : 'a tree -> int
val ( <+> ) : int * int -> int * int -> int * int
val count_node_types : 'a tree -> int * int
val get_elements : 'a tree -> node -> string Set.t
val get_zzp_elements : 'a tree -> node -> ZSet.t
val iter : f:(string -> unit) -> 'a tree -> unit
val count_inmem : node -> int
val count_inmem_tree : 'a tree -> int
val get_inmem_count : 'a tree -> int
val set_inmem_count : 'a tree -> int -> unit
val list_extract : f:('a -> 'b option) -> 'a list -> 'b list
val list_prefix : int -> 'a list -> 'a list
val list_prefix_suffix : int -> 'a list -> 'a list * 'a list
val inmem_children : node -> node list
val get_frontier :
  'a ->
  frontier:node list ->
  newfrontier:node list -> n:int -> count:int -> node list * int
val disconnect_children : node -> unit
val shrink_tree : 'a tree -> 'a option -> int -> unit
val shrink_tree_if_necessary : 'a tree -> 'a option -> unit
val width : int
val rmask : int -> int
val lmask : int -> int
val string_index : 'a tree -> int -> string -> int
val create_svalues : 'a array -> ZZp.mut_array
val incr_inmem_count : 'a tree -> unit
val decr_inmem_count : 'a tree -> unit
val create_node_basic : key -> 'a array -> node
val create_node : 'a tree -> key -> node
val add_to_node : 'a -> node -> 'b -> string -> ZZp.zz array -> unit
val remove_from_node : 'a -> node -> 'b -> string -> ZZp.zz array -> unit
val split_at_depth : 'a tree -> 'b -> 'c -> node -> int -> unit
val pad : string -> int -> string
val create_empty_header :
  points:ZZp.zz array ->
  bitquantum:int ->
  num_samples:int -> thresh:int -> dbopt:'a db option -> 'a tree
val create :
  ?db:(string -> string) * ('a option -> key:string -> data:string -> unit) *
      ('a option -> string -> unit) *
      ((unit -> 'a option) * ('a option -> unit) * ('a option -> unit)) * 
      int ->
  txn:'a option ->
  num_samples:int -> bitquantum:int -> thresh:int -> unit -> 'a tree
val insert_at_depth :
  'a tree -> 'b -> string -> node -> ZZp.zz array -> int -> unit
val insert_both : 'a tree -> 'a option -> ZZp.zz -> string -> unit
val insert : 'a tree -> 'a option -> ZZp.zz -> unit
val insert_str : 'a tree -> 'a option -> string -> unit
val get_ondisk_subkeys : 'a tree -> 'b db -> Bitstring.t -> Bitstring.t Set.t
val delete_at_depth :
  'a tree -> 'a option -> 'b -> string -> node -> ZZp.zz array -> int -> unit
val delete_both : 'a tree -> 'a option -> ZZp.zz -> string -> unit
val delete : 'a tree -> 'a option -> ZZp.zz -> unit
val delete_str : 'a tree -> 'a option -> string -> unit
val set_maxnodes : 'a tree -> 'a option -> int -> unit
val get_maxnodes : 'a tree -> int
val get_node_rec :
  sef:bool ->
  'a tree -> node -> string -> depth:int -> goal_depth:int -> node
val get_node_str : ?sef:bool -> 'a tree -> string -> int -> node
val get_node : ?sef:bool -> 'a tree -> ZZp.zz -> int -> node
val get_node_key : ?sef:bool -> 'a tree -> Bitstring.t -> node
val root : 'a tree -> node
val children : node -> node disk array option
val svalues : node -> ZZp.mut_array
val size : node -> int
val is_leaf : node -> bool
val points : 'a tree -> ZZp.zz array
val elements : 'a tree -> node -> ZSet.t
val node_size : 'a tree -> node disk -> int
val nonempty_children : 'a tree -> node disk array -> int list
val random_element : 'a list -> 'a
val get_random : 'a tree -> node -> string
val set_synctime : 'a tree -> float -> unit
val get_synctime : 'a tree -> float
val depth : 'a tree -> node -> int
val num_elements : 'a -> node -> int
