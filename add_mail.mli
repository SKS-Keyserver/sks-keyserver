module Map :
  sig
    type ('a, 'b) t = ('a, 'b) PMap.Map.t
  end
module Set :
  sig
    type 'a t = 'a PSet.Set.t
  end
val anonymous : string list ref
val usage_string : string
val anon_options : string -> unit
val parse_spec : 'a list
val dirname : string
val pipe_file : in_channel -> out_channel -> unit
val run : unit -> unit
