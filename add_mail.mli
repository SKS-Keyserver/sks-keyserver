module Map :
  sig
    type ('a, 'b) t = ('a, 'b) PMap.Map.t
    val empty : ('a, 'b) t
    val add : key:'a -> data:'b -> ('a, 'b) t -> ('a, 'b) t
    val find : 'a -> ('a, 'b) t -> 'b
    val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
    val mem : 'a -> ('a, 'b) t -> bool
    val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
    val map : f:('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
    val mapi : f:(key:'a -> data:'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    val fold :
      f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
    val of_alist : ('a * 'b) list -> ('a, 'b) t
    val to_alist : ('a, 'b) t -> ('a * 'b) list
  end
module Set :
  sig
    type 'a t = 'a PSet.Set.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val singleton : 'a -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    val subset : 'a t -> 'a t -> bool
    val iter : f:('a -> unit) -> 'a t -> unit
    val fold : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val partition : f:('a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val elements : 'a t -> 'a list
    val min_elt : 'a t -> 'a
    val max_elt : 'a t -> 'a
    val choose : 'a t -> 'a
    val of_list : 'a list -> 'a t
  end
val anonymous : string list ref
val usage_string : string
val anon_options : string -> unit
val parse_spec : 'a list
val dirname : string
val pipe_file : in_channel -> out_channel -> unit
val run : unit -> unit
