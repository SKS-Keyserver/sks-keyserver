exception Error of string
exception LengthError of string
val width : int
type t = { a : string; bitlength : int; }
val bytelength : int -> int
val create : int -> t
val get : t -> int -> int
val lget : t -> int -> bool
val flip : t -> int -> unit
val set : t -> int -> unit
val unset : t -> int -> unit
val setval : t -> int -> bool -> unit
val print : t -> unit
val hexprint : t -> unit
val to_bool_array : t -> bool array
val to_string : t -> string
val to_bytes : t -> string
val of_bytes : string -> int -> t
val of_byte : int -> t
val of_bytes_all : string -> t
val of_int : int -> t
val of_bytes_nocopy : string -> int -> t
val of_bytes_all_nocopy : string -> t
val to_bytes_nocopy : t -> string
val copy : t -> t
val copy_len : t -> int -> t
val shift_pair_left : char -> char -> int -> char
val shift_pair_right : char -> char -> int -> char
val shift_left_small : t -> int -> unit
val shift_right_small : t -> int -> unit
val shift_left : t -> int -> unit
val shift_right : t -> int -> unit
val num_bits : t -> int
val num_bytes : t -> int
val rmasks : int array
val blit : src:t -> dst:t -> len:int -> unit
val zero_out : t -> unit
