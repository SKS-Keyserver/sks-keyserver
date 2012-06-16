exception Empty
type 'a t = { inlist : 'a list; outlist : 'a list; length : int; }
val empty : 'a t
val push : 'a -> 'a t -> 'a t
val enq : 'a -> 'a t -> 'a t
val top : 'a t -> 'a
val pop : 'a t -> 'a * 'a t
val discard : 'a t -> 'a t
val deq : 'a t -> 'a * 'a t
val to_list : 'a t -> 'a list
val length : 'a t -> int
val is_empty : 'a t -> bool
