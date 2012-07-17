val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val iceil : float -> int
val ifloor : float -> int
val bsearch : f:(int -> int) -> low:int -> high:int -> int
val bsearch_val : f:(int -> int * 'a) -> low:int -> high:int -> int * 'a
val is_alnum : char -> bool
val extract_words_rec :
  string -> start:int -> len:int -> string PSet.Set.t -> string PSet.Set.t
val extract_word_set : string -> string PSet.Set.t
val extract_words : string -> string list
val ptest : string -> bool -> unit
val for_loop : int -> int -> 'a -> (int -> 'a -> 'a) -> 'a
val pair_loop : ('a * 'a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val for_all_pairs : ('a -> 'a -> bool) -> 'a list -> bool
val neq_test : 'a * 'a -> bool -> bool
val time : (unit -> 'a) -> float
val random_int : int -> int -> int
val char_width : int
val hexstring : string -> string
val int_from_bstring_rec : string -> pos:int -> len:int -> int -> int
val int_from_bstring : string -> pos:int -> len:int -> int
val bstring_of_int : int -> string
val apply : int -> ('a -> 'a) -> 'a -> 'a
val get_bit : pos:int -> int -> int
val create_rand_bits : unit -> unit -> int
val rbit : unit -> int
val permute : 'a list -> 'a list
exception FinalDouble of exn * exn
exception Final of exn
val try_finally : f:(unit -> 'a) -> finally:(unit -> 'b) -> 'a
val rfold : f:('a -> int -> 'a) -> int -> int -> init:'a -> 'a
val fill_random_string :
  (unit -> int) -> string -> pos:int -> len:int -> unit
val random_string : (unit -> int) -> int -> string
val dedup : 'a list -> 'a list
val unit_memoize : (unit -> 'a) -> unit -> 'a
val memoize : ('a -> 'b) -> 'a -> 'b
val initdbconf : string -> string -> unit
class ['a] memo :
  'a ->
  object
    constraint 'a = 'b -> 'c
    val store : ('b, 'c) MoreLabels.Hashtbl.t
    method apply : 'b -> 'c
    method clear : unit
  end
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
