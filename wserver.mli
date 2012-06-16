module Map :
  sig
    type ('a, 'b) t = ('a, 'b) PMap.Map.t
  end
module Set :
  sig
    type 'a t = 'a PSet.Set.t
  end
exception Page_not_found of string
exception No_results of string
exception Not_implemented of string
(* exception Bad_request of string         *)
(* exception Entity_too_large of string *)
exception Misc_error of string
val ( |= ) : ('a, 'b) Map.t -> 'a -> 'b
val ( |< ) : ('a, 'b) Map.t -> 'a * 'b -> ('a, 'b) Map.t
val hexa_digit : int -> char
val hexa_val : char -> int
val decode : string -> string
val special : char -> bool
val encode : string -> string
val stripchars : char Set.t
val strip : string -> string
type 'a request =
    GET of (string * (string, string) Map.t)
  | POST of (string * (string, string) Map.t * 'a)
val whitespace : Str.regexp
val eol : Str.regexp
val get_all : in_channel -> string
val get_lines : in_channel -> string list
val max_post_length : int
val parse_post : (string, string) Map.t -> in_channel -> string
val is_blank : string -> bool
val parse_headers :
  (string, string) Map.t -> in_channel -> (string, string) Map.t
val parse_request : in_channel -> string request
val headers_to_string : (string, string) Map.t -> string
val request_to_string : 'a request -> string
val request_to_string_short : 'a request -> string
val send_result :
  out_channel ->
  ?error_code:int -> ?content_type:string -> ?count:int -> string -> unit
val accept_connection :
  ('a -> string request -> Channel.out_channel_obj -> string * int) ->
  recover_timeout:int -> 'a -> in_channel -> out_channel -> 'b list
