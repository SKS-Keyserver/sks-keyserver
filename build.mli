module F :
  functor (M : sig  end) ->
    sig
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
      val settings : Keydb.dbsettings
      module Keydb :
        sig
          type txn = Keydb.Safe.txn
          val open_dbs : Keydb.dbsettings -> unit
          val close_dbs : unit -> unit
          val sync : unit -> unit
          val txn_begin : ?parent:txn -> unit -> txn option
          val txn_commit : txn option -> unit
          val txn_abort : txn option -> unit
          val checkpoint : unit -> unit
          val unconditional_checkpoint : unit -> unit
          val get_num_keys : unit -> int
          val get_dump_filearray : unit -> in_channel array
          val get_by_words : max:int -> string list -> Packet.key list
          val get_by_hash : string -> Packet.key
          val get_keystring_by_hash : string -> string
          val iter : f:(hash:string -> key:Packet.key -> 'a) -> unit
          val keyiter : f:(string -> 'a) -> unit
          val get_by_short_subkeyid : string -> Packet.key list
          val logquery : ?maxsize:int -> float -> (float * Common.event) list
          val reverse_logquery :
            ?maxsize:int -> float -> (float * Common.event) list
          val create_hashstream :
            unit -> string SStream.sstream * (unit -> unit)
          val create_hash_skey_stream :
            unit -> (string * string) SStream.sstream * (unit -> unit)
          val last_ts : unit -> float
          val enqueue_key : txn:txn option -> Packet.key -> unit
          val dequeue_key : txn:txn option -> float * Packet.key
          type 'a offset = 'a Keydb.Safe.offset = { fnum : int; pos : 'a; }
          and skey =
            Keydb.Safe.skey =
              KeyString of string
            | Key of Packet.packet list
            | Offset of int offset
            | LargeOffset of int64 offset
          type key_metadata =
            Keydb.Safe.key_metadata = {
            md_hash : string;
            md_words : string list;
            md_keyid : string;
            md_subkey_keyids : string list;
            md_time : float;
            md_skey : skey;
          }
          val key_to_metadata : ?hash:Digest.t -> Packet.key -> key_metadata
          val key_to_metadata_large_offset :
            int64 offset -> Packet.packet list -> key_metadata
          val add_mds : key_metadata list -> unit
          val add_key :
            ?parent:txn -> ?hash:Digest.t -> Packet.packet list -> unit
          val add_keys : Packet.packet list list -> unit
          val add_key_merge : newkey:bool -> Packet.packet list -> unit
          val add_keys_merge : Packet.packet list list -> unit
          val swap_keys : Packet.packet list -> Packet.packet list -> unit
          val get_meta : string -> string
          val set_meta : key:string -> data:string -> unit
          val replace : Packet.packet list list -> Packet.packet list -> unit
          val delete_key : ?hash:'a -> Packet.packet list -> unit
        end
      val n : int
      val fnames : string list
      val get_keys_rec :
        (unit -> Packet.packet list option) ->
        Packet.packet list list -> Packet.packet list list
      val get_keys :
        (unit -> Packet.packet list option) -> Packet.packet list list
      val timestr : float -> string
      val nsplit : int -> 'a list -> 'a list * 'a list
      val batch_iter : f:('a list -> 'b) -> int -> 'a list -> unit
      val get_keys_fname :
        string -> Packet.packet list list -> Packet.packet list list
      val get_keys_multi : string list -> Packet.packet list list
      val dbtimer : MTimer.t
      val timer : MTimer.t
      val run : unit -> unit
    end
