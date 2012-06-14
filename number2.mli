val two : Big_int.big_int
val one : Big_int.big_int
val zero : Big_int.big_int
val neg_one : Big_int.big_int
val ( *! ) : Big_int.big_int -> Big_int.big_int -> Big_int.big_int
val ( +! ) : Big_int.big_int -> Big_int.big_int -> Big_int.big_int
val ( -! ) : Big_int.big_int -> Big_int.big_int -> Big_int.big_int
val ( %! ) :
  Big_int.big_int -> Big_int.big_int -> Big_int.big_int * Big_int.big_int
val ( /! ) : Big_int.big_int -> Big_int.big_int -> Big_int.big_int
val ( **! ) : Big_int.big_int -> Big_int.big_int -> Big_int.big_int
val ( <>! ) : Big_int.big_int -> Big_int.big_int -> bool
val ( =! ) : Big_int.big_int -> Big_int.big_int -> bool
val ( <! ) : Big_int.big_int -> Big_int.big_int -> bool
val ( >! ) : Big_int.big_int -> Big_int.big_int -> bool
val ( <=! ) : Big_int.big_int -> Big_int.big_int -> bool
val ( >=! ) : Big_int.big_int -> Big_int.big_int -> bool
val width : int
val width_pow : Big_int.big_int
val revstring : string -> string
val revstring_inplace : string -> unit
val bigint_to_bytes : nbytes:int -> Big_int.big_int -> string
val bigint_of_bytes : string -> Big_int.big_int
module type ZZpType =
  sig
    type t
    type tref
    type zzarray
    val nbits : int
    val nbytes : int
    val of_bytes : string -> t
    val to_bytes : t -> string
    val of_int : int -> t
    val to_N : t -> Big_int.big_int
    val of_N : Big_int.big_int -> t
    val one : t
    val zero : t
    val add : t -> t -> t
    val div : t -> t -> t
    val mul : t -> t -> t
    val mult : t -> t -> t
    val inv : t -> t
    val neg : t -> t
    val shl : t -> int -> t
    val imult : t -> int -> t
    val add_fast : t -> t -> t
    val mul_fast : t -> t -> t
    val mult_fast : t -> t -> t
    val square : t -> t
    val square_fast : t -> t
    val canonicalize : t -> t
    val sub : t -> t -> t
    val print : t -> unit
    val imul : t -> int -> t
    val lt : t -> t -> bool
    val gt : t -> t -> bool
    val eq : t -> t -> bool
    val neq : t -> t -> bool
    val look : tref -> t
    val mult_in : tref -> t -> t -> unit
    val mult_fast_in : tref -> t -> t -> unit
    val add_in : tref -> t -> t -> unit
    val add_fast_in : tref -> t -> t -> unit
    val sub_in : tref -> t -> t -> unit
    val sub_fast_in : tref -> t -> t -> unit
    val copy_in : tref -> t -> unit
    val copy_out : tref -> t
    val make_ref : t -> tref
    val canonicalize_in : tref -> unit
    val points : int -> t array
    val svalues : int -> zzarray
    val to_string : t -> string
    val add_el_array : points:t array -> t -> t array
    val del_el_array : points:t array -> t -> t array
    val mult_array : svalues:zzarray -> t array -> unit
    val add_el : svalues:zzarray -> points:t array -> t -> unit
    val del_el : svalues:zzarray -> points:t array -> t -> unit
    val length : zzarray -> int
    val zzarray_to_array : zzarray -> t array
    val zzarray_of_array : t array -> zzarray
    val zzarray_div : zzarray -> zzarray -> zzarray
    val zzarray_copy : zzarray -> zzarray
    val cmp : t -> t -> int
    val order : Big_int.big_int
  end
