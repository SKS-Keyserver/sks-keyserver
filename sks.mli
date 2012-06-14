type command = {
  name : string;
  usage : string;
  desc : string;
  func : unit -> unit;
}
val usage : command -> string
val space : Str.regexp
val commands : command list
val help : unit -> unit
val find : string -> command list -> command
