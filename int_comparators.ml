(* rename the polymorphic comparators *)
let ( <>: ) = ( <> )
let ( =: ) = ( = )
let ( <: ) = ( < )
let ( >: ) = ( > )
let ( <=: ) = ( <= )
let ( >=: ) = ( >= )

(* and then constraint the usual ones to ints *)
let ( <> ) (x :int) y : bool = x <> y
let ( = ) (x :int) y : bool = x = y
let ( < ) (x :int) y : bool = x < y
let ( > ) (x :int) y : bool = x > y
let ( <= ) (x :int) y : bool = x <= y
let ( >= ) (x :int) y : bool = x >= y
