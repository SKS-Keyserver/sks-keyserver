exception Bug of string
type 'a bottomQ_entry
type reconbound
exception Continue
val send_request :
  < outchan : out_channel; write_int : int -> 'a; .. > ->
  'b PrefixTree.tree ->
  bottomQ:(PrefixTree.node * Bitstring.t) bottomQ_entry Queue.t ->
  PrefixTree.node * Bitstring.t -> unit
val handle_reply :
  < outchan : out_channel; write_int : int -> 'a; .. > ->
  'b PrefixTree.tree ->
  requestQ:(PrefixTree.node * Bitstring.t) Queue.t ->
  ReconMessages.msg_container ->
  PrefixTree.node * Bitstring.t -> ZZp.Set.t ref -> unit
val connection_manager :
  < fd : UnixLabels.file_descr; read_int : int;
    read_string : int -> string; .. > ->
  < flush : 'a; outchan : out_channel; write_int : int -> 'b; .. > ->
  'c PrefixTree.tree -> PrefixTree.node * Bitstring.t -> ZZp.Set.t
val handle :
  'a PrefixTree.tree ->
  < fd : UnixLabels.file_descr; read_int : int;
    read_string : int -> string; .. > ->
  < flush : 'b; outchan : out_channel; write_int : int -> 'c; .. > ->
  ZZp.Set.t
