(* A functor that builds the main function for an executable that builds up the key
   database from a multi-file database dump.  dump files are taken from the
   command-line *)

module F (M : sig end) : sig
  val run : unit -> unit
end
