val whitespace : Str.regexp
val eol : Str.regexp
val parse_header_line : string -> (string * string) option
val parse_header :
  string list ->
  (string * string) list -> (string * string) list * string list
val simplify_headers : (string * string) list -> (string * string) list
val parse : string -> Sendmail.msg
