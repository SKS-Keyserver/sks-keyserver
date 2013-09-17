type recon_rqst_poly = {
  rp_prefix : Bitstring.t;
  rp_size : int;
  rp_samples : ZZp.mut_array;
}

type recon_rqst_full = { rf_prefix : Bitstring.t; rf_elements : ZZp.Set.t; }

type configdata = (string, string) PMap.Map.t

type msg =
  | ReconRqst_Poly of recon_rqst_poly
  | ReconRqst_Full of recon_rqst_full
  | Elements of ZZp.Set.t
  | FullElements of ZZp.Set.t
  | SyncFail
  | Done
  | Flush
  | Error of string
  | DbRqst of string
  | DbRepl of string
  | Config of configdata

val msg_to_string : msg -> string

module M : sig
  type msg_container = { msg : msg; }
end
type msg_container = M.msg_container = { msg : msg; }
val marshal_noflush :
  < outchan : out_channel; write_int : int -> 'a; .. > -> msg -> unit
val marshal :
  < flush : 'a; outchan : out_channel; write_int : int -> 'b; .. > ->
  msg -> 'a
val try_unmarshal :
  < fd : UnixLabels.file_descr; read_int : int;
    read_string : int -> string; .. > ->
  msg_container option
val unmarshal :
  < read_int : int; read_string : int -> string; .. > -> msg_container
val sockaddr_to_string : Unix.sockaddr -> string
