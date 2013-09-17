/***********************************************************************)
(* bdb_stubs.c - Stubs appropriate for Berkeley DB 4.x and DB 5.x      *)
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
(***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <limits.h>
#include <db.h>
/* O_CREAT and others are not defined in db.h */
#include <fcntl.h>

#include <string.h>
#include "bdb_stubs.h"

#ifndef DB_XA_CREATE
#define DB_XA_CREATE 0
#endif

#define True 1
#define False 0


void zerob(void* addr,size_t n) {
  memset(addr,0,n);
}

#define test_cursor_closed(cursor) \
  if (UW_cursor_closed(cursor)) \
   invalid_argument("Attempt to use closed cursor")

#define test_dbenv_closed(dbenv) \
  if (UW_dbenv_closed(dbenv)) \
   invalid_argument("Attempt to use closed dbenv")

#define test_db_closed(db) \
  if (UW_db_closed(db))  \
   invalid_argument("Attempt to use closed db")

#define test_txn_closed(txn) \
  if (UW_txn_closed(txn)) \
    invalid_argument("Attempt to use closed txn")

// comments starting with "//+" are extracted automatically to create the .ml
// file that forms the caml side of this interface.

/************************************************************/
/***  Custom Operations *************************************/
/************************************************************/

// ###### DB_ENV ######

#define caml_dbenv_close_internal(dbenv) \
  (!(UW_dbenv_closed(dbenv)) ? \
    UW_dbenv_closed(dbenv) = True, \
    UW_dbenv(dbenv)->close(UW_dbenv(dbenv),0) : \
   0 )

static void finalize_caml_dbenv(value dbenv) {
  //fprintf(stderr,"GC: Finalizing Dbenv\n"); fflush(stderr);
  caml_dbenv_close_internal(dbenv);
  //fprintf(stderr,"GC: Dbenv Finalized\n"); fflush(stderr);
}

static struct custom_operations dbenv_custom = {
  "sks.bdb.dbenv",
  finalize_caml_dbenv,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

// ###### DB ######

#define caml_db_close_internal(db) \
  (!(UW_db_closed(db)) ? \
   UW_db_closed(db) = True, \
   UW_db(db)->close(UW_db(db),0) : \
   0 )

static void finalize_caml_db(value db) {
  //fprintf(stderr,"GC: Finalizing Db\n"); fflush(stderr);
  caml_db_close_internal(db);
  //fprintf(stderr,"GC: Db Finalized\n"); fflush(stderr);
}

static struct custom_operations db_custom = {
  "sks.bdb.db",
  finalize_caml_db,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

// ###### Cursor ######

#define caml_cursor_close_internal(cursor) \
  (!(UW_cursor_closed(cursor)) ? \
    (UW_cursor_closed(cursor) = True, \
     UW_cursor(cursor)->c_close(UW_cursor(cursor))) : \
    0 )

static void finalize_caml_cursor(value cursor) {
  //fprintf(stderr,"GC: Finalizing Cursor\n"); fflush(stderr);
  caml_cursor_close_internal(cursor);
  //fprintf(stderr,"GC: Cursor Finalized\n"); fflush(stderr);
}

static struct custom_operations cursor_custom = {
  "sks.bdb.cursor",
  finalize_caml_cursor,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

// ###### Transaction ######

// ###### Cursor ######

#define caml_cursor_close_internal(cursor) \
  (!(UW_cursor_closed(cursor)) ? \
    (UW_cursor_closed(cursor) = True, \
     UW_cursor(cursor)->c_close(UW_cursor(cursor))) : \
    0 )

static void finalize_caml_txn(value txn) {
  //fprintf(stderr,"GC: Finalizing Txn\n"); fflush(stderr);

  /* Try to abort any transaction that gets GC'd
     without being closed first */
  if (!UW_txn_closed(txn)) {
    //fprintf(stderr,"GC: Aborting unclosed transaction\n");
    //fflush(stderr);
    UW_txn(txn)->abort(UW_txn(txn));
  }

  //fprintf(stderr,"GC: Txn Finalized\n"); fflush(stderr);
}

static struct custom_operations txn_custom = {
  "sks.bdb.txn",
  finalize_caml_txn,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/************************************************************/
/************ Exception buckets *****************************/
/************************************************************/

static value *caml_db_exn = NULL;
static value *caml_key_exists_exn = NULL;
static value *caml_db_run_recovery_exn = NULL;

value caml_db_init(value v){
  CAMLparam1(v);
  if (caml_db_exn == NULL)
    caml_db_exn = caml_named_value("dberror");
  if (caml_key_exists_exn == NULL)
    caml_key_exists_exn = caml_named_value("keyexists");
  if (caml_db_run_recovery_exn == NULL)
    caml_db_run_recovery_exn = caml_named_value("dbrunrecovery");
  CAMLreturn (Val_unit);
}

//+ (* GENERATED FILE -- DO NOT EDIT -- see bdb_stubs.c *)
//+
//+ (* Exception declarations *)
//+
//+ exception DBError of string
//+ let _ = Callback.register_exception "dberror" (DBError "")
//+
//+ exception Key_exists
//+ let _ = Callback.register_exception "keyexists" Key_exists
//+
//+ exception Run_recovery
//+ let _ = Callback.register_exception "dbrunrecovery" Run_recovery
//+
//+ external db_init : unit -> unit = "caml_db_init"
//+ let _ = db_init ()
//+
//+ type txn
//+ type cursor
//+ type dbenv
//+ type db

void raise_db(const char *msg) {
  raise_with_string(*caml_db_exn, msg);
}

void raise_key_exists() {
  raise_constant(*caml_key_exists_exn);
}

void raise_run_recovery() {
  raise_constant(*caml_db_run_recovery_exn);
}

// Used as callback by db infrastructure for setting errors.  As a result,
// calls to DB->err and DBENV->err lead to exceptions.

// FIX: currently, prefix is ignored.  Should be concatenated.
void raise_db_cb(const DB_ENV *dbenv, const char *prefix, const char *msg) {
    raise_db(msg);
}

//+ external version : unit -> string = "caml_db_version"
value caml_db_version() {
  int major, minor, patch;
  char version[10];

  db_version(&major, &minor, &patch);
  sprintf(version, "%d.%d.%d", major, minor, patch);

  return caml_copy_string(version);
}

// #############################################################
// Opening of Dbenv moudle
//+
//+
//+ module Dbenv =
//+ struct
//+
//+   type t = dbenv


/**  DBENV Flags  ********************************************/

// Declaration of flag enums in ocaml must be in same order as in C

static int dbenv_open_flags[] = {
  DB_JOINENV, DB_INIT_CDB, DB_INIT_LOCK, DB_INIT_LOG, DB_INIT_MPOOL,
  DB_INIT_TXN, DB_RECOVER, DB_RECOVER_FATAL, DB_USE_ENVIRON,
  DB_USE_ENVIRON_ROOT, DB_CREATE, DB_LOCKDOWN, DB_PRIVATE,
  DB_SYSTEM_MEM, DB_THREAD
};

//+
//+   type open_flag =
//+       JOINENV | INIT_CDB | INIT_LOCK | INIT_LOG
//+     | INIT_MPOOL | INIT_TXN | RECOVER | RECOVER_FATAL
//+     | USE_ENVIRON | USE_ENVIRON_ROOT | CREATE
//+     | LOCKDOWN | PRIVATE | SYSTEM_MEM | THREAD

static int dbenv_verbose_flags[] = {
  DB_VERB_DEADLOCK, DB_VERB_RECOVERY, DB_VERB_WAITSFOR
};

//+
//+   type verbose_flag =
//+       VERB_CHKPOINT | VERB_DEADLOCK | VERB_RECOVERY | VERB_WAITSFOR

/**  DBENV Calls  *******************************************/
//+

//+   external create : unit -> t = "caml_dbenv_create"
value caml_dbenv_create(value unit){
  CAMLparam1(unit);
  CAMLlocal1(rval);
  int err;
  int flags = 0;
  DB_ENV *dbenv;

  err = db_env_create(&dbenv,flags);
  if (err != 0) { raise_db(db_strerror(err)); }

  dbenv->set_errcall(dbenv,raise_db_cb);

  rval = alloc_custom(&dbenv_custom,Camldbenv_wosize,0,1);
  UW_dbenv(rval) = dbenv;
  UW_dbenv_closed(rval) = False;
  CAMLreturn (rval);
}


//+   external dopen : t -> string -> open_flag list -> int -> unit =
//+        "caml_dbenv_open"
value caml_dbenv_open(value dbenv, value vdirectory,
                      value vflags, value vmode){
  CAMLparam4(dbenv,vdirectory,vflags,vmode);
  int err;
  char *directory = String_val(vdirectory);
  int flags = convert_flag_list(vflags,dbenv_open_flags);

  test_dbenv_closed(dbenv);

  err = UW_dbenv(dbenv)->open(UW_dbenv(dbenv), directory,
                              flags,
                              Long_val(vmode) );
  if (err != 0) {
    UW_dbenv(dbenv)->err(UW_dbenv(dbenv),err,
                         "caml_dbenv_open: open failed.");
  }

  CAMLreturn (Val_unit);
}
// simple open, combination of create and open
//+   let sopen dirname flags mode =
//+     let dbenv = create () in
//+     dopen dbenv dirname flags mode;
//+     dbenv

char db_message[255];

void db_msgcall_fcn(const DB_ENV *dbenv, const char *msg)
{
        if(strlen(msg) < 254)
        strcpy(db_message, msg);
}

//+             external get_dbenv_stats : t -> string = "caml_dbenv_get_stats"
value caml_dbenv_get_stats(value dbenv){
        CAMLparam1(dbenv);

        char output_message[255];
        char nl[] = {"\n"};
        int err;

        UW_dbenv(dbenv)->set_msgcall(UW_dbenv(dbenv), *db_msgcall_fcn);
        err = UW_dbenv(dbenv)->stat_print(UW_dbenv(dbenv), DB_STAT_ALL);
        if(err == 0){
                if(strlen(db_message) < 253){
                    strcpy(output_message, db_message);
                    strcat(output_message, nl);
                }

                UW_dbenv(dbenv)->stat_print(UW_dbenv(dbenv), DB_STAT_ALL | DB_STAT_SUBSYSTEM);
                if(strlen(output_message) + strlen(db_message) < 253){
                    strcat(output_message, db_message);
                    strcat(output_message, nl);
                }
        }
        else
        {
                strcpy(output_message, "Unable to open environment");
        }

    return caml_copy_string(output_message);
}

//+   external close : t -> unit = "caml_dbenv_close"
value caml_dbenv_close(value dbenv) {
  CAMLparam1(dbenv);
  int err;

  //fprintf(stderr,"Closing Dbenv\n"); fflush(stderr);
  err = caml_dbenv_close_internal(dbenv);
  if (err != 0) { raise_db(db_strerror(err)); }
  //fprintf(stderr,"Dbenv Closed\n"); fflush(stderr);

  CAMLreturn (Val_unit);
}


//+   external set_verbose_internal : t -> verbose_flag list ->
//+           bool -> unit =  "caml_dbenv_set_verbose"
//+   let set_verbose dbenv flag onoff =
//+       set_verbose_internal dbenv [flag] onoff
value caml_dbenv_set_verbose(value dbenv, value vflags,
                             value v_onoff) {
  CAMLparam3(dbenv,vflags,v_onoff);
  int err;

  int which = convert_flag_list(vflags,dbenv_verbose_flags) + 1;
  int onoff = Bool_val(v_onoff);

  test_dbenv_closed(dbenv);

  err = UW_dbenv(dbenv)->set_verbose(UW_dbenv(dbenv),which,onoff);

  if (err != 0) {
    UW_dbenv(dbenv)->err(UW_dbenv(dbenv),err,
                         "caml_dbenv_set_verbose:");
  }
  CAMLreturn (Val_unit);
}

//+   external set_cachesize : t -> gbytes:int -> bytes:int ->
//+          ncache:int -> unit = "caml_dbenv_set_cachesize"
value caml_dbenv_set_cachesize(value dbenv, value gbytes,
                               value bytes, value ncache) {
  CAMLparam4(dbenv, gbytes, bytes, ncache);
  int err;

  err = UW_dbenv(dbenv)->set_cachesize(UW_dbenv(dbenv),Int_val(gbytes),
                                       Int_val(bytes), Int_val(ncache));
  if (err != 0) { UW_dbenv(dbenv)->err(UW_dbenv(dbenv),err,
                                       "caml_dbenv_set_cachesize"); }

  CAMLreturn (Val_unit);
}



// Termination of Dbenv module
//+
//+ end


// #############################################################
// Opening of Db moudle
//+
//+
//+ module Db =
//+ struct
//+
//+   type t = db



/**  DB Flags  ***********************************************/
static int db_create_flags[] = {
};

//+
//+   type create_flag

static int db_open_flags[] = {
  DB_CREATE, DB_EXCL, DB_NOMMAP, DB_RDONLY, DB_THREAD,
  DB_TRUNCATE, DB_AUTO_COMMIT
};

//+
//+   type open_flag =
//+      CREATE | EXCL | NOMMAP | RDONLY | THREAD | TRUNCATE | AUTO_COMMIT

static int db_types[] = {
  DB_BTREE, DB_HASH, DB_QUEUE, DB_RECNO, DB_UNKNOWN
};

//+
//+   type db_type = BTREE | HASH | QUEUE | RECNO | UNKNOWN


static int db_put_flags[] = { DB_APPEND, DB_NODUPDATA, DB_NOOVERWRITE };

//+
//+   type put_flag = APPEND | NODUPDATA | NOOVERWRITE

// DB_GET_BOTH is omitted because it doesn't make sense given our interface
static int db_get_flags[] = {
  DB_CONSUME, DB_CONSUME_WAIT, DB_SET_RECNO, DB_RMW
};

//+
//+   type get_flag = CONSUME | CONSUME_WAIT | SET_RECNO | RMW


static int db_set_flags[] = {
  DB_DUP, DB_DUPSORT, DB_RECNUM, DB_REVSPLITOFF, DB_RENUMBER, DB_SNAPSHOT
};

//+
//+   type set_flag = DUP | DUPSORT | RECNUM | REVSPLITOFF
//+                 | RENUMBER | SNAPSHOT

/** DB Calls **************************************************/
//+

//+   external create : ?dbenv:Dbenv.t -> create_flag list -> t =
//+        "caml_db_create"
value caml_db_create(value dbenv_opt, value vflags){
  CAMLparam2(dbenv_opt,vflags);
  int err;
  int flags;
  DB *db;
  DB_ENV *dbenv;
  CAMLlocal1(rval);

  /* The flags parameter is currently unused, and must be set to 0. */
  if (vflags != Val_emptylist)
    invalid_argument("DB.create invalid create flag");
  flags = convert_flag_list(vflags,db_create_flags);

  if (Is_None(dbenv_opt)) { dbenv = NULL; }
  else {
    test_dbenv_closed(Some_val(dbenv_opt));
    dbenv = UW_dbenv(Some_val(dbenv_opt));
  }

  err = db_create(&db,dbenv,flags);
  if (err != 0) { raise_db(db_strerror(err)); }

  db->set_errcall(db,raise_db_cb);

  rval = alloc_custom(&db_custom,Camldb_wosize,0,1);
  UW_db(rval) = db;
  UW_db_closed(rval) = False;
  CAMLreturn (rval);

}

//+   external dopen : t -> string -> db_type -> open_flag list
//+        -> int -> unit =  "caml_db_open"
value caml_db_open(value db, value vfname,
                   value vdbtype, value vflags,
                   value vmode){
  CAMLparam5(db, vfname, vdbtype, vflags, vmode);
  int err;
  char *fname = String_val(vfname);
  int flags = convert_flag_list(vflags,db_open_flags);
  int dbtype = Flag_val(vdbtype,db_types);

  test_db_closed(db);

  err = UW_db(db)->open(UW_db(db),
                        NULL,
                        fname,
                        NULL, /* no support for multiple databases in
                                 a single file */
                        dbtype,
                        flags, /* automatic transaction on database open */
                        Long_val(vmode) );
  if (err != 0) {
    UW_db(db)->err(UW_db(db),err,
                         "caml_db_open");
  }

  CAMLreturn (Val_unit);
}

//+   external close : t -> unit = "caml_db_close"
value caml_db_close(value db) {
  CAMLparam1(db);
  int err;

  //fprintf(stderr,"Closing Dbenv\n"); fflush(stderr);
  err = caml_db_close_internal(db);
  if (err != 0) { raise_db(db_strerror(err)); }
  //fprintf(stderr,"Dbenv Closed\n"); fflush(stderr);

  CAMLreturn (Val_unit);
}

//+   external del : t -> ?txn:txn -> string -> unit = "caml_db_del"
value caml_db_del(value db, value txn_opt, value key) {
  CAMLparam3(db,txn_opt,key);
  DBT dbt; // static keyword initializes record to zero.
  int err;
  DB_TXN *txn;

  if (Is_None(txn_opt)) { txn = NULL; }
  else {
    test_txn_closed(Some_val(txn_opt));
    txn = UW_txn(Some_val(txn_opt));
  }

  test_db_closed(db);

  zerob(&dbt,sizeof(DBT));

  dbt.data = String_val(key);
  dbt.size = string_length(key);


  err = UW_db(db)->del(UW_db(db), txn, &dbt, 0);
  if (err != 0) { UW_db(db)->err(UW_db(db),err, "caml_db_del"); }

  CAMLreturn (Val_unit);
}


//+   external put : t -> ?txn:txn -> key:string -> data:string
//+             -> put_flag list -> unit = "caml_db_put"
value caml_db_put(value db, value txn_opt, value vkey,
                  value vdata, value vflags) {
  CAMLparam5(db, txn_opt, vkey, vdata, vflags);
  DBT key, data;
  int flags, err;
  DB_TXN *txn;

  if (Is_None(txn_opt)) { txn = NULL; }
  else {
    test_txn_closed(Some_val(txn_opt));
    txn = UW_txn(Some_val(txn_opt));
  }

  test_db_closed(db);

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  key.data = String_val(vkey);
  key.size = string_length(vkey);
  data.data = String_val(vdata);
  data.size = string_length(vdata);
  flags = convert_flag_list(vflags, db_put_flags);

  err = UW_db(db)->put(UW_db(db), txn, &key, &data, flags);
  if (err != 0) {
    if (err  == DB_KEYEXIST) {raise_key_exists();}
    UW_db(db)->err(UW_db(db),err,"caml_db_put");
  }

  CAMLreturn (Val_unit);
}


//+   external get : t -> ?txn:txn -> string -> get_flag list -> string
//+             = "caml_db_get"
value caml_db_get(value db, value txn_opt, value vkey, value vflags) {
  CAMLparam4(db, txn_opt, vkey, vflags);
  DBT key,data;
  int flags, err;
  DB_TXN *txn;
  CAMLlocal1(rval);

  if (Is_None(txn_opt)) { txn = NULL; }
  else {
    test_txn_closed(Some_val(txn_opt));
    txn = UW_txn(Some_val(txn_opt));
  }

  test_db_closed(db);

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  key.data = String_val(vkey);
  key.size = string_length(vkey);
  flags = convert_flag_list(vflags, db_get_flags);


  err = UW_db(db)->get(UW_db(db), txn, &key, &data, flags);
  if (err != 0) {
    ////fprintf(stderr,"Error found: %d\n",err); fflush(stderr);
    if (err == DB_NOTFOUND) { raise_not_found(); }
    UW_db(db)->err(UW_db(db),err,"caml_db_get");
  }

  // FIX: this currently uses an extra, unnecessary copy in order to simplify
  // memory management.
  rval = alloc_string(data.size);
  memcpy (String_val(rval), data.data, data.size);
  CAMLreturn (rval);
}

//+   external set_flags : t -> set_flag list -> unit = "caml_db_set_flags"
value caml_db_set_flags(value db, value vflags) {
  CAMLparam2(db,vflags);
  int flags=0,err;

  test_db_closed(db);

  flags = convert_flag_list(vflags,db_set_flags);

  err = UW_db(db)->set_flags(UW_db(db),flags);
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_set_flags"); }

  CAMLreturn (Val_unit);
}


//  More user-friendly version of dopen (simple open)
//+
//+   let sopen ?dbenv fname dbtype ?moreflags flags mode =
//+     let db = create ?dbenv [] in
//+     (match moreflags with
//+         None -> ()
//+       | Some flags -> set_flags db flags );
//+     dopen db fname dbtype flags mode;
//+     db



//+   external set_h_ffactor : t -> int -> unit
//+          = "caml_db_set_h_ffactor"
value caml_db_set_h_ffactor(value db, value v) {
  CAMLparam2(db,v);
  int err;

  test_db_closed(db);

  err = UW_db(db)->set_h_ffactor(UW_db(db),Int_val(v));
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_set_h_ffactor"); }

  CAMLreturn (Val_unit);
}

//+   external set_pagesize : t -> int -> unit
//+          = "caml_db_set_pagesize"
value caml_db_set_pagesize(value db, value v) {
  CAMLparam2(db,v);
  int err;

  test_db_closed(db);

  err = UW_db(db)->set_pagesize(UW_db(db),Int_val(v));
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_set_pagesize"); }

  CAMLreturn (Val_unit);
}

//+   external set_cachesize : t -> gbytes:int -> bytes:int
//+          -> ncache:int -> unit = "caml_db_set_cachesize"
value caml_db_set_cachesize(value db, value gbytes, value bytes, value ncache) {
  CAMLparam4(db, gbytes, bytes, ncache);
  int err;

  test_db_closed(db);

  err = UW_db(db)->set_cachesize(UW_db(db),Int_val(gbytes), Int_val(bytes),
                                 Int_val(ncache));
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_set_cachesize"); }

  CAMLreturn (Val_unit);
}


//+   external sync : t -> unit = "caml_db_sync"
value caml_db_sync(value db) {
  CAMLparam1(db);
  int err;

  test_db_closed(db);
  err = UW_db(db)->sync(UW_db(db),0);
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_sync"); }

  CAMLreturn (Val_unit);
}


//+   external get_size : t -> int = "caml_db_get_size"
value caml_db_get_size(value db) {
  CAMLparam1(db);
  int err;
  void *stat;
  int size = 0;
  DB_TXN *txn = NULL;

  test_db_closed(db);
  err = UW_db(db)->stat(UW_db(db),txn,&stat,0);
  if (err != 0) { UW_db(db)->err(UW_db(db),err,"caml_db_get_size"); }
  switch (*(u_int32_t*)stat) {
  case DB_BTREEMAGIC:
    size = ((DB_BTREE_STAT*)stat)->bt_ndata;
    break;
  case DB_HASHMAGIC:
    size = ((DB_HASH_STAT*)stat)->hash_ndata;
    break;
  case DB_QAMMAGIC:
    size = ((DB_QUEUE_STAT*)stat)->qs_ndata;
    break;
  default:
    break;
  }

  free(stat);
  CAMLreturn (Val_int(size));
}

// Termination of Db module
//+
//+ end
//+

//*******************************************************************
//*******************************************************************

// #############################################################
// Opening of Cursor moudle
//+
//+ module Cursor =
//+ struct
//+
//+   type t = cursor

//*******************************************************************
//*******************************************************************

static int cursor_put_flags[] = {
  DB_AFTER, DB_BEFORE, DB_CURRENT
};

//+
//+   type put_flag = AFTER | BEFORE | CURRENT

static int cursor_kput_flags[] = {
  DB_KEYFIRST, DB_KEYLAST, DB_NODUPDATA
};

//+
//+   type kput_flag = KEYFIRST | KEYLAST | NODUPDATA

static int cursor_get_type[] = {
  DB_CURRENT, DB_FIRST, DB_LAST,
  DB_NEXT, DB_PREV, DB_NEXT_DUP, DB_NEXT_NODUP, DB_PREV_NODUP, 0
};

//+
//+   type get_type = CURRENT | FIRST | LAST
//+          | NEXT | PREV | NEXT_DUP | NEXT_NODUP
//+          | PREV_NODUP | NULL

static int cursor_get_flags[] = { DB_RMW };

//+
//+   type get_flag = RMW

//*******************************************************************
//*******************************************************************

//+   (* Note: A cursor created with a transaction must be closed before
//+      the transaction is committed or aborted *)
//+   external create : ?writecursor:bool -> ?txn:txn -> Db.t -> t
//+               = "caml_cursor_create"
value caml_cursor_create(value vwritecursor, value txn_opt, value db) {
  CAMLparam3(vwritecursor,txn_opt,db);
  int err;
  int flags = 0;
  CAMLlocal1(rval);
  DBC *cursor;
  DB_TXN *txn;

  if (Is_None(txn_opt)) { txn = NULL; }
  else {
    test_txn_closed(Some_val(txn_opt));
    txn = UW_txn(Some_val(txn_opt));
  }

  test_db_closed(db);

  // setup flags from vwritecursor
  if (Is_Some(vwritecursor) && Bool_val(Some_val(vwritecursor))) {
    flags = DB_WRITECURSOR;
  }

  //  printf("%d\n",ctr++); fflush(stdout);

  err = UW_db(db)->cursor(UW_db(db),txn,&cursor,flags);
  if (err != 0) {
    UW_db(db)->err(UW_db(db),err, "caml_cursor_create");
  }

  rval = alloc_custom(&cursor_custom,Camlcursor_wosize,0,1);

  UW_cursor(rval) = cursor;
  UW_cursor_closed(rval) = False;
  CAMLreturn (rval);
}

//+   external close : t -> unit = "caml_cursor_close"
value caml_cursor_close(value cursor) {
  CAMLparam1(cursor);
  int err;

  //fprintf(stderr,"Closing Dbenv\n"); fflush(stderr);
  err = caml_cursor_close_internal(cursor);
  if (err != 0) { raise_db(db_strerror(err)); }
  //fprintf(stderr,"Dbenv Closed\n"); fflush(stderr);

  CAMLreturn (Val_unit);
}

//+   external put : t -> string -> put_flag -> unit
//+          = "caml_cursor_put"
value caml_cursor_put(value cursor, value vdata, value vflag) {
  CAMLparam3(cursor,vdata,vflag);
  DBT key, data;
  int flags, err;

  test_cursor_closed(cursor);

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  data.data = String_val(vdata);
  data.size = string_length(vdata);
  flags = Flag_val(vflag, cursor_put_flags);

  err = UW_cursor(cursor)->c_put(UW_cursor(cursor), &key, &data, flags);
  if (err != 0) {
    if (err == DB_KEYEXIST) { raise_key_exists(); }
    raise_db(db_strerror(err));
  }

  CAMLreturn (Val_unit);
}

//+   external kput : t -> key:string -> data:string -> kput_flag -> unit
//+          = "caml_cursor_kput"
value caml_cursor_kput(value cursor, value vkey, value vdata, value vflag) {
  CAMLparam4(cursor,vkey,vdata,vflag);
  DBT key, data;
  int flags, err;

  test_cursor_closed(cursor);

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  key.data = String_val(vkey);
  key.size = string_length(vkey);
  data.data = String_val(vdata);
  data.size = string_length(vdata);
  flags = Flag_val(vflag,cursor_kput_flags);

  err = UW_cursor(cursor)->c_put(UW_cursor(cursor), &key, &data, flags);
  if (err != 0) {
    if (err == DB_KEYEXIST) { raise_key_exists(); }
    raise_db(db_strerror(err));
  }

  CAMLreturn (Val_unit);
}


//+   external init :  t -> string -> get_flag list -> string
//+          = "caml_cursor_init"
value caml_cursor_init(value cursor, value vkey, value vflags) {
  CAMLparam3(cursor,vkey,vflags);
  CAMLlocal1(rval);
  DBT key,data;
  int flags = convert_flag_list(vflags,cursor_get_flags) | DB_SET;
  int err;

  test_cursor_closed(cursor);

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  key.data = String_val(vkey);
  key.size = string_length(vkey);

  err = UW_cursor(cursor)->c_get(UW_cursor(cursor), &key, &data, flags);
  if (err != 0) {
    if (err == DB_NOTFOUND) { raise_not_found(); }
    raise_db(db_strerror(err));
  }

  rval = alloc_string(data.size);
  memcpy (String_val(rval), data.data, data.size);
  CAMLreturn (rval);
}


//+   external init_range :  t -> string -> get_flag list -> string * string
//+          = "caml_cursor_init_range"
value caml_cursor_init_range(value cursor, value vkey, value vflags) {
  CAMLparam3(cursor,vkey,vflags);
  CAMLlocal3(rkey,rdata,rpair);
  DBT key,data;
  int flags = convert_flag_list(vflags,cursor_get_flags) | DB_SET_RANGE;
  int err;

  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  test_cursor_closed(cursor);

  key.data = String_val(vkey);
  key.size = string_length(vkey);

  err = UW_cursor(cursor)->c_get(UW_cursor(cursor), &key, &data, flags);
  if (err != 0) {
    if (err == DB_NOTFOUND) { raise_not_found(); }
    raise_db(db_strerror(err));
  }

  rdata = alloc_string(data.size);
  memcpy (String_val(rdata), data.data, data.size);

  rkey = alloc_string(key.size);
  memcpy (String_val(rkey), key.data, key.size);

  rpair = alloc(2,0);

  Store_field(rpair,0,rkey);
  Store_field(rpair,1,rdata);

  CAMLreturn (rpair);
}

//+   external init_both :  t -> key:string -> data:string
//+               -> get_flag list -> unit = "caml_cursor_init_both"
value caml_cursor_init_both(value cursor, value vkey,
                            value vdata , value vflags
                            ) {
   CAMLparam4(cursor,vkey,vdata,vflags);
   DBT key,data;
   int flags;
   int err;

   int ctr = 0;

   flags = convert_flag_list(vflags,cursor_get_flags) | DB_GET_BOTH;
   test_cursor_closed(cursor);

   zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

   key.data = String_val(vkey);
   key.size = string_length(vkey);

   data.data = String_val(vdata);
   data.size = string_length(vdata);

   err = UW_cursor(cursor)->c_get(UW_cursor(cursor), &key, &data, flags);
   if (err != 0) {
     if (err == DB_NOTFOUND) { raise_not_found (); }
     raise_db(db_strerror(err));
   }

   CAMLreturn (Val_unit);
}


//+   external get : t -> get_type -> get_flag list -> string * string
//+                = "caml_cursor_get"
value caml_cursor_get(value cursor, value vtype, value vflags) {
  CAMLparam3(cursor,vtype,vflags);
  CAMLlocal3(rpair,rkey,rdata);
  DBT key,data;
  int flags = Flag_val(vtype,cursor_get_type) |
    convert_flag_list(vflags,cursor_get_flags);
  int err;
  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  test_cursor_closed(cursor);

  err = UW_cursor(cursor)->c_get(UW_cursor(cursor), &key, &data,flags);
  if (err != 0) {
    if (err == DB_NOTFOUND) { raise_not_found(); }
    raise_db(db_strerror(err));
  }

  rkey = alloc_string(key.size);
  memcpy (String_val(rkey), key.data, key.size);
  rdata = alloc_string(data.size);
  memcpy (String_val(rdata), data.data, data.size);
  rpair = alloc(2,0);
  Store_field(rpair,0,rkey);
  Store_field(rpair,1,rdata);
  CAMLreturn (rpair);
}


//+   external get_keyonly : t -> get_type -> get_flag list -> string
//+                = "caml_cursor_get_keyonly"
value caml_cursor_get_keyonly(value cursor, value vtype, value vflags) {
  CAMLparam3(cursor,vtype,vflags);
  CAMLlocal1(rkey);
  DBT key,data;
  int flags = Flag_val(vtype,cursor_get_type) |
    convert_flag_list(vflags,cursor_get_flags);
  int err;
  zerob(&key,sizeof(DBT)); zerob(&data,sizeof(DBT));

  test_cursor_closed(cursor);

  err = UW_cursor(cursor)->c_get(UW_cursor(cursor), &key, &data,flags);
  if (err != 0) {
    if (err == DB_NOTFOUND) { raise_not_found(); }
    raise_db(db_strerror(err));
  }

  rkey = alloc_string(key.size);
  memcpy (String_val(rkey), key.data, key.size);
  CAMLreturn (rkey);
}



//+   external del : t -> unit = "caml_cursor_del"
value caml_cursor_del(value cursor) {
  CAMLparam1(cursor);
  int err;

  test_cursor_closed(cursor);

  err = UW_cursor(cursor)->c_del(UW_cursor(cursor), 0);
  if (err != 0) { raise_db(db_strerror(err)); }

  CAMLreturn (Val_unit);
}


//+   external count : t -> int = "caml_cursor_count"
value caml_cursor_count(value cursor) {
  CAMLparam1(cursor);
  int err;
  db_recno_t counter;

  test_cursor_closed(cursor);

  err = UW_cursor(cursor)->c_count(UW_cursor(cursor), &counter,0);
  if (err != 0) { raise_db(db_strerror(err)); }

  CAMLreturn (Val_long(counter));
}

//+   external dup : ?keep_position:bool -> t -> t = "caml_cursor_dup"
value caml_cursor_dup(value vkeep_position, value cursor) {
  CAMLparam2(vkeep_position,cursor);
  CAMLlocal1(rval);
  int flags = 0, err;
  DBC *newcursor;

  test_cursor_closed(cursor);

  if (Is_Some(vkeep_position) && Bool_val(vkeep_position)) {
    flags = DB_POSITION;
  }

  err = UW_cursor(cursor)->c_dup(UW_cursor(cursor), &newcursor, flags);
  if (err != 0) { raise_db(db_strerror(err)); }

  rval = alloc_custom(&cursor_custom,Camlcursor_wosize,0,1);
  UW_cursor(rval) = newcursor;
  UW_cursor_closed(rval) = False;

  CAMLreturn (rval);
}


//+   external ajoin : ?nosort:bool -> db -> cursor array -> get_flag list ->
//+                       cursor = "caml_join_cursors"
//+   let join ?nosort  db cursor_list get_flag_list =
//+        ajoin ?nosort db (Array.of_list cursor_list) get_flag_list
value caml_join_cursors(value vnosort, value db,
                        value vcursors, value vflags) {
  CAMLparam4(vnosort,db,vcursors,vflags);
  CAMLlocal1(rval);
  DBC *jcurs; // pointer to joined cursor
  int carray_len = Wosize_val(vcursors);
  int flags = convert_flag_list(vflags,cursor_get_flags);
  DBC *cursors[carray_len + 1];
  int i;

  if (Is_Some(vnosort) && Bool_val(vnosort)) {
    flags = flags | DB_JOIN_NOSORT;
  }

  for (i=0; i < carray_len; i++) {
    if (UW_cursor_closed(Field(vcursors,i))) {
      invalid_argument("caml_join_cursors: Attempt to use closed cursor");
    }
    cursors[i] = UW_cursor(Field(vcursors,i));
  }
  cursors[i] = NULL;
  test_db_closed(db);

  UW_db(db)->join(UW_db(db),cursors,&jcurs,flags);


  rval = alloc_custom(&cursor_custom,Camlcursor_wosize,0,1);
  UW_cursor(rval) = jcurs;
  UW_cursor_closed(rval) = False;
  CAMLreturn (rval);
}

// Termination of Cursor module
//+
//+ end
//+


// #############################################################
// Opening of Transaction module
//+
//+ module Txn =
//+ struct
//+
//+   type t = txn


static int txn_begin_flags[] = {
  /* DB_DIRTY_READ, */ DB_TXN_NOSYNC, DB_TXN_NOWAIT, DB_TXN_SYNC
};

//+
//+   type begin_flag = (* DIRTY_READ | *) NOSYNC | NOWAIT | SYNC

static int txn_checkpoint_flags[] = { DB_FORCE };

//+
//+   type checkpoint_flag = FORCE

static int txn_commit_flags[] = { DB_TXN_NOSYNC, DB_TXN_SYNC };

//+
//+   type commit_flag = COM_NOSYNC | COM_SYNC


//+
//+   (* set max # of active transactions *)
//+   external set_txn_max : dbenv -> int -> unit = "caml_set_txn_max"
value caml_set_txn_max(value dbenv, value vmax) {
  CAMLparam2(dbenv,vmax);
  int err;
  int max = Int_val(vmax);

  test_dbenv_closed(dbenv);

  err = UW_dbenv(dbenv)->set_tx_max(UW_dbenv(dbenv),max);
  if (err != 0) {
    //fprintf(stderr,"Error found: %d\n",err); fflush(stderr);
    if (err == EINVAL) {
      invalid_argument("set_txn_max called after dbenv opened");
    } else {
      UW_dbenv(dbenv)->err(UW_dbenv(dbenv), err, "caml_set_txn_max");
    }
  }

  CAMLreturn(Val_unit);

}

//+   external abort : t -> unit = "caml_txn_abort"
value caml_txn_abort(value txn) {
  CAMLparam1(txn);
  int err;

  test_txn_closed(txn);

  err = UW_txn(txn)->abort(UW_txn(txn));
  UW_txn_closed(txn) = True;
  if (err != 0) {
    //fprintf(stderr,"Error found: %d\n",err); fflush(stderr);
    if (err == DB_RUNRECOVERY) { raise_run_recovery(); }
    else { raise_db(db_strerror(err)); }
  }

  CAMLreturn(Val_unit);
}

//+   external txn_begin : dbenv -> t option -> begin_flag list -> t
//+        = "caml_txn_begin"
value caml_txn_begin(value dbenv, value parent_opt, value vflags) {
  CAMLparam3(dbenv,parent_opt,vflags);
  CAMLlocal1(rval);
  int err,flags;
  DB_TXN *parent, *newtxn;

  test_dbenv_closed(dbenv);

  flags = convert_flag_list(vflags,txn_begin_flags);

  if (Is_None(parent_opt)) { parent = NULL; }
  else {
    test_txn_closed(Some_val(parent_opt));
    parent = UW_txn(Some_val(parent_opt));
    //printf("********* parented transaction ***************\n"); fflush(stdout);
  }

  err = UW_dbenv(dbenv)->txn_begin(UW_dbenv(dbenv), parent, &newtxn, flags);
  if (err != 0) {
    if (err == ENOMEM) {
      failwith("Maximum # of concurrent transactions reached");
    } else {
      UW_dbenv(dbenv)->err(UW_dbenv(dbenv), err,"caml_txn_begin");
    }
  }

  rval = alloc_custom(&txn_custom,Camltxn_wosize,0,1);
  UW_txn(rval) = newtxn;
  UW_txn_closed(rval) = False;
  CAMLreturn(rval);
}


//+   external checkpoint: dbenv -> kbyte:int -> min:int
//+       -> checkpoint_flag list -> unit = "caml_txn_checkpoint"
value caml_txn_checkpoint(value dbenv, value vkbyte, value vmin,
                          value vflags) {
  CAMLparam4(dbenv,vkbyte,vmin,vflags);
  int err, kbyte, min, flags;

  test_dbenv_closed(dbenv);

  kbyte = Int_val(vkbyte);
  min = Int_val(vmin);
  flags = convert_flag_list(vflags,txn_checkpoint_flags);

  err = UW_dbenv(dbenv)->txn_checkpoint(UW_dbenv(dbenv),kbyte,min,flags);
  if (err != 0) {
    //fprintf(stderr,"Error found: %d\n",err); fflush(stderr);
    if (err == EINVAL) {
      invalid_argument("caml_txn_checkpoint: no reason specified");
    } else {
      UW_dbenv(dbenv)->err(UW_dbenv(dbenv), err, "caml_txn_checkpoint");
    }
  }

  CAMLreturn(Val_unit);
}

//+   external commit: t -> commit_flag list -> unit = "caml_txn_commit"
value caml_txn_commit(value txn, value vflags) {
  CAMLparam2(txn,vflags);
  int err, flags;

  test_txn_closed(txn);
  flags = convert_flag_list(vflags,txn_commit_flags);

  err = UW_txn(txn)->commit(UW_txn(txn),flags);
  UW_txn_closed(txn) = True; // transaction can never be used again

  if (err != 0) {
    //fprintf(stderr,"Error found: %d\n",err); fflush(stderr);
    if (err == DB_RUNRECOVERY) raise_run_recovery();
    else raise_db(db_strerror(err));
  }

  CAMLreturn(Val_unit);
}

// Termination of Txn module
//+
//+ end
//+
