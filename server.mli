exception Bug of string
val solving : float ref
val lookup : float ref
val flushtime : float ref
val unmarsh_time : float ref
val solve :
  remote_size:int ->
  local_size:int ->
  remote_samples:ZZp.mut_array ->
  local_samples:ZZp.mut_array ->
  points:ZZp.zz array -> (ZZp.Set.t * ZZp.Set.t) option
val handle_one :
  'a PrefixTree.tree ->
  < read_int : int; read_string : int -> string; .. > ->
  < flush : 'b; outchan : out_channel; write_int : int -> 'c; .. > ->
  bool * ZZp.Set.t
val recover_timeout : int
val handle :
  'a PrefixTree.tree ->
  < read_int : int; read_string : int -> string; .. > ->
  < flush : 'b; outchan : out_channel; write_int : int -> 'c; .. > -> ZZp.Set.t
