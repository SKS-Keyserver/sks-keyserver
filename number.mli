type z
module Infix :
sig
  val two : z
  val one : z
  val zero : z
  val neg_one : z
  val ( *! ) : z -> z -> z
  val ( +! ) : z -> z -> z
  val ( -! ) : z -> z -> z
  val ( %! ) : z -> z -> z
  val ( /! ) : z -> z -> z
  val ( **! ) : z -> int -> z
  val ( <>! ) : z -> z -> bool
  val ( =! ) : z -> z -> bool
  val ( <! ) : z -> z -> bool
  val ( >! ) : z -> z -> bool
  val ( <=! ) : z -> z -> bool
  val ( >=! ) : z -> z -> bool
end
val width : int
val width_pow : z
val nbits : z -> int
val nth_bit : z -> int -> bool
val print_bits : z -> unit
val squaremod : z -> z -> z
val powmod : z -> z -> z -> z
val dumb_powmod : z -> z -> z -> z
val gcd_ex : z -> z -> z * z * z

val int_mult : int -> z -> z
val int_posint_power : int -> int -> z

(** conversion functions *)

val to_bytes : nbytes:int -> z -> string
val of_bytes : string -> z
val of_int : int -> z
val to_int : z -> int
val to_string : z -> string
val of_string : string -> z
val compare : z -> z -> int
