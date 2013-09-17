exception Key_exists
module Dbenv :
  sig
    type t = Bdb.dbenv
    type open_flag =
      Bdb.Dbenv.open_flag =
        JOINENV
      | INIT_CDB
      | INIT_LOCK
      | INIT_LOG
      | INIT_MPOOL
      | INIT_TXN
      | RECOVER
      | RECOVER_FATAL
      | USE_ENVIRON
      | USE_ENVIRON_ROOT
      | CREATE
      | LOCKDOWN
      | PRIVATE
      | SYSTEM_MEM
      | THREAD
    type verbose_flag =
      Bdb.Dbenv.verbose_flag =
        VERB_CHKPOINT
      | VERB_DEADLOCK
      | VERB_RECOVERY
      | VERB_WAITSFOR
    val create : unit -> t
    val dopen : t -> string -> open_flag list -> int -> unit
    val sopen : string -> open_flag list -> int -> t
    val close : t -> unit
    val set_verbose_internal : t -> verbose_flag list -> bool -> unit
    val set_verbose : t -> verbose_flag -> bool -> unit
    val set_cachesize : t -> gbytes:int -> bytes:int -> ncache:int -> unit
  end
module Db :
  sig
    type t = Bdb.db
    type create_flag = Bdb.Db.create_flag
    type open_flag =
      Bdb.Db.open_flag =
        CREATE
      | EXCL
      | NOMMAP
      | RDONLY
      | THREAD
      | TRUNCATE
      | AUTO_COMMIT
    type db_type = Bdb.Db.db_type = BTREE | HASH | QUEUE | RECNO | UNKNOWN
    type put_flag = Bdb.Db.put_flag = APPEND | NODUPDATA | NOOVERWRITE
    type get_flag =
      Bdb.Db.get_flag =
        CONSUME
      | CONSUME_WAIT
      | SET_RECNO
      | RMW
    type set_flag =
      Bdb.Db.set_flag =
        DUP
      | DUPSORT
      | RECNUM
      | REVSPLITOFF
      | RENUMBER
      | SNAPSHOT
    external get_size : t -> int = "caml_db_get_size"
    val create : ?dbenv:Bdb.Dbenv.t -> create_flag list -> t
    val dopen : t -> string -> db_type -> open_flag list -> int -> unit
    val close : t -> unit
    val del : t -> ?txn:Bdb.txn -> string -> unit
    val put :
      t -> ?txn:Bdb.txn -> key:string -> data:string -> put_flag list -> unit
    val get : t -> ?txn:Bdb.txn -> string -> get_flag list -> string
    val set_flags : t -> set_flag list -> unit
    val sopen :
      ?dbenv:Bdb.Dbenv.t ->
      string ->
      db_type -> ?moreflags:set_flag list -> open_flag list -> int -> t
    val set_h_ffactor : t -> int -> unit
    val set_pagesize : t -> int -> unit
    val set_cachesize : t -> gbytes:int -> bytes:int -> ncache:int -> unit
    val sync : t -> unit
  end
module Cursor :
  sig
    type t = Bdb.cursor
    type put_flag = Bdb.Cursor.put_flag = AFTER | BEFORE | CURRENT
    type kput_flag = Bdb.Cursor.kput_flag = KEYFIRST | KEYLAST | NODUPDATA
    type get_type =
      Bdb.Cursor.get_type =
        CURRENT
      | FIRST
      | LAST
      | NEXT
      | PREV
      | NEXT_DUP
      | NEXT_NODUP
      | PREV_NODUP
      | NULL
    type get_flag = Bdb.Cursor.get_flag = RMW
    val create : ?writecursor:bool -> ?txn:Bdb.txn -> Bdb.Db.t -> t
    val close : t -> unit
    val put : t -> string -> put_flag -> unit
    val kput : t -> key:string -> data:string -> kput_flag -> unit
    val init : t -> string -> get_flag list -> string
    val init_range : t -> string -> get_flag list -> string * string
    val init_both : t -> key:string -> data:string -> get_flag list -> unit
    val get : t -> get_type -> get_flag list -> string * string
    val get_keyonly : t -> get_type -> get_flag list -> string
    val del : t -> unit
    val count : t -> int
    val dup : ?keep_position:bool -> t -> t
    val ajoin :
      ?nosort:bool ->
      Bdb.db -> Bdb.cursor array -> get_flag list -> Bdb.cursor
    val join :
      ?nosort:bool ->
      Bdb.db -> Bdb.cursor list -> get_flag list -> Bdb.cursor
  end
module Txn :
  sig
    type t = Bdb.txn
    type begin_flag = Bdb.Txn.begin_flag = NOSYNC | NOWAIT | SYNC
    type checkpoint_flag = Bdb.Txn.checkpoint_flag = FORCE
    type commit_flag = Bdb.Txn.commit_flag = COM_NOSYNC | COM_SYNC
    val set_txn_max : Bdb.dbenv -> int -> unit
    val abort : t -> unit
    val txn_begin : Bdb.dbenv -> t option -> begin_flag list -> t
    val checkpoint :
      Bdb.dbenv -> kbyte:int -> min:int -> checkpoint_flag list -> unit
    val commit : t -> commit_flag list -> unit
  end
