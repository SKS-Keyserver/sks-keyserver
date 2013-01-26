(***********************************************************************)
(* temp.ml                                                             *)
(*                                                                     *)
(* Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, *)
(*               2011, 2012, 2013  Yaron Minsky and Contributors       *)
(*                                                                     *)
(* This file is part of SKS.  SKS is free software; you can            *)
(* redistribute it and/or modify it under the terms of the GNU General *)
(* Public License as published by the Free Software Foundation; either *)
(* version 2 of the License, or (at your option) any later version.    *)
(*                                                                     *)
(* This program is distributed in the hope that it will be useful, but *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *)
(* General Public License for more details.                            *)
(*                                                                     *)
(* You should have received a copy of the GNU General Public License   *)
(* along with this program; if not, write to the Free Software         *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA or see <http://www.gnu.org/licenses/>.                          *)
(***********************************************************************)

(* Exception declarations *)

exception DBError of string
let _ = Callback.register_exception "dberror" (DBError "")

exception Key_exists
let _ = Callback.register_exception "keyexists" Key_exists

exception Run_recovery
let _ = Callback.register_exception "dbrunrecovery" Run_recovery

external db_init : unit -> unit = "caml_db_init"
let _ = db_init ()

type txn
type cursor
type dbenv
type db


module Dbenv =
struct

  type t = dbenv

  type create_flag = CLIENT

  type open_flag =
      JOINENV | INIT_CDB | INIT_LOCK | INIT_LOG
    | INIT_MPOOL | INIT_TXN | RECOVER | RECOVER_FATAL
    | USE_ENVIRON | USE_ENVIRON_ROOT | CREATE
    | LOCKDOWN | PRIVATE | SYSTEM_MEM | THREAD

  type verbose_flag =
      VERB_CHKPOINT | VERB_DEADLOCK | VERB_RECOVERY | VERB_WAITSFOR

  external create : create_flag list -> t = "caml_dbenv_create"
  external dopen : t -> string -> open_flag list -> int -> unit =
       "caml_dbenv_open"
  let sopen dirname flags mode =
    let dbenv = create [] in
    dopen dbenv dirname flags mode;
    dbenv
  external close : t -> unit = "caml_dbenv_close"
  external set_verbose_internal : t -> verbose_flag list ->
          bool -> unit =  "caml_dbenv_set_verbose"
  let set_verbose dbenv flag onoff =
      set_verbose_internal dbenv [flag] onoff
  external set_cachesize : t -> gbytes:int -> bytes:int ->
         ncache:int -> unit = "caml_dbenv_set_cachesize"

end


module Db =
struct

  type t = db

  type create_flag = XA_CREATE

  type open_flag =
     CREATE | EXCL | NOMMAP | RDONLY | THREAD | TRUNCATE

  type db_type = BTREE | HASH | QUEUE | RECNO | UNKNOWN

  type put_flag = APPEND | NODUPDATA | NOOVERWRITE

  type get_flag = CONSUME | CONSUME_WAIT | SET_RECNO | RMW

  type set_flag = DUP | DUPSORT | RECNUM | REVSPLITOFF
                | RENUMBER | SNAPSHOT

  external create : ?dbenv:Dbenv.t -> create_flag list -> t =
       "caml_db_create"
  external dopen : t -> string -> db_type -> open_flag list
       -> int -> unit =  "caml_db_open"
  external close : t -> unit = "caml_db_close"
  external del : t -> ?txn:txn -> string -> unit = "caml_db_del"
  external put : t -> ?txn:txn -> key:string -> data:string
            -> put_flag list -> unit = "caml_db_put"
  external get : t -> ?txn:txn -> string -> get_flag list -> string
            = "caml_db_get"
  external set_flags : t -> set_flag list -> unit = "caml_db_set_flags"

  let sopen ?dbenv fname dbtype ?moreflags flags mode =
    let db = create ?dbenv [] in
    (match moreflags with
        None -> ()
      | Some flags -> set_flags db flags );
    dopen db fname dbtype flags mode;
    db
  external set_h_ffactor : t -> int -> unit
         = "caml_db_set_h_ffactor"
  external set_pagesize : t -> int -> unit
         = "caml_db_set_pagesize"
  external set_cachesize : t -> gbytes:int -> bytes:int
         -> ncache:int -> unit = "caml_db_set_cachesize"
  external sync : t -> unit = "caml_db_sync"

end


module Cursor =
struct

  type t = cursor

  type put_flag = AFTER | BEFORE | CURRENT

  type kput_flag = KEYFIRST | KEYLAST | NODUPDATA

  type get_type = CURRENT | FIRST | LAST
         | NEXT | PREV | NEXT_DUP | NEXT_NODUP
         | PREV_NODUP | NULL

  type get_flag = RMW
  (* Note: A cursor created with a transaction must be closed before
     the transaction is committed or aborted *)
  external create : ?writecursor:bool -> ?txn:txn -> Db.t -> t
              = "caml_cursor_create"
  external close : t -> unit = "caml_cursor_close"
  external put : t -> string -> put_flag -> unit
         = "caml_cursor_put"
  external kput : t -> key:string -> data:string -> kput_flag -> unit
         = "caml_cursor_kput"
  external init :  t -> string -> get_flag list -> string
         = "caml_cursor_init"
  external init_range :  t -> string -> get_flag list -> string * string
         = "caml_cursor_init_range"
  external init_both :  t -> key:string -> data:string
              -> get_flag list -> unit = "caml_cursor_init_both"
  external get : t -> get_type -> get_flag list -> string * string
               = "caml_cursor_get"
  external get_keyonly : t -> get_type -> get_flag list -> string
               = "caml_cursor_get_keyonly"
  external del : t -> unit = "caml_cursor_del"
  external count : t -> int = "caml_cursor_count"
  external dup : ?keep_position:bool -> t -> t = "caml_cursor_dup"
  external ajoin : ?nosort:bool -> db -> cursor array -> get_flag list ->
                      cursor = "caml_join_cursors"
  let join ?nosort  db cursor_list get_flag_list =
       ajoin ?nosort db (Array.of_list cursor_list) get_flag_list

end


module Txn =
struct

  type t = txn

  type begin_flag = (* DIRTY_READ | *) NOSYNC | NOWAIT | SYNC

  type checkpoint_flag = FORCE

  type commit_flag = COM_NOSYNC | COM_SYNC

  (* set max # of active transactions *)
  external set_txn_max : dbenv -> int -> unit = "caml_set_txn_max"
  external abort : t -> unit = "caml_txn_abort"
  external txn_begin : dbenv -> t option -> begin_flag list -> t
       = "caml_txn_begin"
  external checkpoint: dbenv -> kbyte:int -> min:int
      -> checkpoint_flag list -> unit = "caml_txn_checkpoint"
  external commit: t -> commit_flag list -> unit = "caml_txn_commit"

end

