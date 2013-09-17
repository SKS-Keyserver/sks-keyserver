val settings : Keydb.dbsettings
module Keydb :
  sig
    type txn = Bdb.txn
    val word_db_name : string
    val key_db_name : string
    val keyid_db_name : string
    val subkey_keyid_db_name : string
    val time_db_name : string
    val tqueue_db_name : string
    val meta_db_name : string
    val max_internal_matches : int
    type action = Keydb.Unsafe.action = DeleteKey | AddKey
    type 'a offset = 'a Keydb.Unsafe.offset = { fnum : int; pos : 'a; }
    type skey =
      Keydb.Unsafe.skey =
        KeyString of string
      | Key of Packet.packet list
      | Offset of int offset
      | LargeOffset of int64 offset
    type dbdump =
      Keydb.Unsafe.dbdump = {
      directory : string;
      filearray : in_channel array;
    }
    type dbstate =
      Keydb.Unsafe.dbstate = {
      settings : Keydb.dbsettings;
      dbenv : Bdb.Dbenv.t;
      key : Bdb.Db.t;
      word : Bdb.Db.t;
      keyid : Bdb.Db.t;
      subkey_keyid : Bdb.Db.t;
      time : Bdb.Db.t;
      tqueue : Bdb.Db.t;
      meta : Bdb.Db.t;
      dump : dbdump;
    }
    val dbstate : dbstate option ref
    exception No_db
    val get_dbs : unit -> dbstate
    val get_dump_filearray : unit -> in_channel array
    val marshal_offset : < write_int : int -> 'a; .. > -> int offset -> 'a
    val unmarshal_offset : < read_int : int; .. > -> int offset
    val marshal_large_offset :
      < write_int : int -> 'a; write_int64 : 'b -> 'c; .. > ->
      'b offset -> 'c
    val unmarshal_large_offset :
      < read_int : int; read_int64 : 'a; .. > -> 'a offset
    val skey_of_string : string -> skey
    val skey_to_string : skey -> string
    val skey_is_offset : skey -> bool
    val keystring_of_offset :
      [< `large_offset of 'a offset & int64 offset
       | `offset of 'a offset & int offset ] ->
      string
    val keystring_of_skey : skey -> string
    val keystring_of_string : string -> string
    val key_of_skey : skey -> Packet.packet list
    val key_to_string : Packet.packet list -> string
    val key_of_string : string -> Packet.packet list
    val read_dir_suff : string -> string -> string list
    val open_dbs : Keydb.dbsettings -> unit
    val close_dump : dbstate -> unit
    val close_dbs : unit -> unit
    val sync : unit -> unit
    val txn_begin : ?parent:Bdb.Txn.t -> unit -> Bdb.Txn.t option
    val txn_commit : Bdb.Txn.t option -> unit
    val txn_abort : Bdb.Txn.t option -> unit
    val checkpoint : unit -> unit
    val unconditional_checkpoint : unit -> unit
    val float_to_string : float -> string
    val float_of_string : string -> float
    val event_to_string : Common.event -> string
    val event_of_string : string -> Common.event
    val flatten_array_of_lists : 'a list array -> 'a array
    val jcursor_get_all : max:int -> Bdb.Cursor.t -> string list
    val get_by_words : max:int -> string list -> Packet.packet list list
    val get_skeystring_by_hash : string -> string
    val get_keystring_by_hash : string -> string
    val get_by_hash : string -> Packet.packet list
    val has_hash : string -> bool
    val check_word_hash_pair : word:string -> hash:string -> bool
    val check_keyid_hash_pair : keyid:string -> hash:string -> bool
    val get_keystrings_by_hashes : string list -> string list
    val keyid_iter : f:(keyid:string -> hash:string -> 'a) -> unit
    val raw_iter : f:(hash:string -> keystr:string -> 'a) -> unit
    val iter : f:(hash:string -> key:Packet.packet list -> 'a) -> unit
    val keyiter : f:(string -> 'a) -> unit
    val get_hashes_by_keyid : Bdb.Db.t -> string -> string list
    val get_skeystrings_by_keyid : Bdb.Db.t -> string -> string list
    val get_by_short_keyid : string -> Packet.packet list list
    val get_by_short_subkeyid : string -> Packet.packet list list
    val logquery : ?maxsize:int -> float -> (float * Common.event) list
    val reverse_logquery :
      ?maxsize:int -> float -> (float * Common.event) list
    val create_hashstream : unit -> string SStream.sstream * (unit -> unit)
    val create_hash_skey_stream :
      unit -> (string * string) SStream.sstream * (unit -> unit)
    val last_ts : unit -> float
    val enqueue_key : txn:Bdb.txn option -> Packet.packet list -> unit
    val dequeue_key : txn:Bdb.txn option -> float * Packet.packet list
    type key_metadata =
      Keydb.Unsafe.key_metadata = {
      md_hash : string;
      md_words : string list;
      md_keyid : string;
      md_subkey_keyids : string list;
      md_time : float;
      md_skey : skey;
    }
    val shorten_offset : int64 offset -> skey
    val key_to_metadata_large_offset :
      int64 offset -> Packet.packet list -> key_metadata
    val key_to_metadata_offset :
      int offset -> Packet.packet list -> key_metadata
    val key_to_metadata :
      ?hash:Digest.t -> Packet.packet list -> key_metadata
    val add_mds : key_metadata list -> unit
    val apply_md_updates_txn :
      txn:Bdb.txn option -> (key_metadata * action) array -> unit
    val apply_md_updates : (key_metadata * action) array -> unit
    val add_md_txn : ?txn:Bdb.txn -> key_metadata -> unit
    val add_key_txn :
      ?txn:Bdb.txn -> ?hash:Digest.t -> Packet.packet list -> unit
    val add_key :
      ?parent:Bdb.Txn.t -> ?hash:Digest.t -> Packet.packet list -> unit
    val add_multi_keys : Packet.packet list list -> unit
    val add_keys : Packet.packet list list -> unit
    val key_to_merge_updates :
      Packet.packet list -> (key_metadata * action) list
    val sort_remove :
      (key_metadata * action) list -> (key_metadata * action) list
    val add_keys_merge_txn :
      txn:Bdb.txn option -> Packet.packet list list -> int
    val add_keys_merge : Packet.packet list list -> unit
    val add_key_merge : newkey:bool -> Packet.packet list -> unit
    val delete_key_txn :
      ?txn:Bdb.txn -> ?hash:Digest.t -> Packet.packet list -> unit
    val swap_keys : Packet.packet list -> Packet.packet list -> unit
    val delete_key : ?hash:'a -> Packet.packet list -> unit
    val get_meta : string -> string
    val set_meta_txn :
      txn:Bdb.txn option -> key:string -> data:string -> unit
    val set_meta : key:string -> data:string -> unit
    val replace : Packet.packet list list -> Packet.packet list -> unit
    val get_num_keys : unit -> int
  end
type update = { keyid : string; hash : string; }
val ( |= ) : ('a, 'b) PMap.Map.t -> 'a -> 'b
val ( |< ) : ('a, 'b) PMap.Map.t -> 'a * 'b -> ('a, 'b) PMap.Map.t
val at_once : int
val subkeyids_from_key : Packet.packet list -> string list
val sort_dedup : 'a list -> 'a list
val apply_updates : update list -> unit
val fix_keyids : unit -> unit
val run : unit -> unit
