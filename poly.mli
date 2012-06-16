val rfind : f:(int -> bool) -> int -> int -> int
type t = { a : ZZp.zz array; degree : int; }
val compute_degree : ZZp.zz array -> int
val init : int -> f:(int -> ZZp.zz) -> t
val make : int -> ZZp.zz -> t
val zero : t
val one : t
val degree : t -> int
val length : t -> int
val copy : t -> t
val to_string : t -> string
val splitter : Str.regexp
val parse_digit : string -> int * ZZp.zz
val map_keys : ('a, 'b) PMap.Map.t -> 'a list
val of_string : string -> t
val print : t -> unit
exception NotEqual
val eq : t -> t -> bool
val of_array : ZZp.zz array -> t
val term : int -> ZZp.zz -> t
val set_length : int -> t -> t
val to_array : t -> ZZp.zz array
val is_monic : t -> bool
val eval : t -> ZZp.zz -> ZZp.zz
val mult : t -> t -> t
val scmult : t -> ZZp.zz -> t
val add : t -> t -> t
val neg : t -> t
val sub : t -> t -> t
val divmod : t -> t -> t * t
val modulo : t -> t -> t
val div : t -> t -> t
val const_coeff : t -> ZZp.zz
val nth_coeff : t -> int -> ZZp.zz
val const : ZZp.zz -> t
val gcd_rec : t -> t -> t
val gcd : t -> t -> t
