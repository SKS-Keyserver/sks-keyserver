val amp : Str.regexp
val chsplit : char -> string -> string * string
val eqsplit : string -> string * string
type request_kind = VIndex | Index | Get | HGet | Stats
type request = {
  kind : request_kind;
  search : string list;
  fingerprint : bool;
  hash : bool;
  exact : bool;
  machine_readable : bool;
  clean : bool;
  limit : int;
}
val default_request : request
val comma_rxp : Str.regexp
val request_of_oplist : ?request:request -> (string * string) list -> request
