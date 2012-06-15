module Map :
  sig
    type ('a, 'b) t = ('a, 'b) PMap.Map.t
    val empty : ('a, 'b) t
    val add : key:'a -> data:'b -> ('a, 'b) t -> ('a, 'b) t
    val find : 'a -> ('a, 'b) t -> 'b
    val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
    val mem : 'a -> ('a, 'b) t -> bool
    val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
    val map : f:('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
    val mapi : f:(key:'a -> data:'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
    val fold :
      f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
    val of_alist : ('a * 'b) list -> ('a, 'b) t
    val to_alist : ('a, 'b) t -> ('a * 'b) list
  end
module Set :
  sig
    type 'a t = 'a PSet.Set.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val singleton : 'a -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    val subset : 'a t -> 'a t -> bool
    val iter : f:('a -> unit) -> 'a t -> unit
    val fold : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val partition : f:('a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val elements : 'a t -> 'a list
    val min_elt : 'a t -> 'a
    val max_elt : 'a t -> 'a
    val choose : 'a t -> 'a
    val of_list : 'a list -> 'a t
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
