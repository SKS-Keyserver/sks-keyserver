(***********************************************************************)
(* pTreeDB.ml                                                          *)
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

open StdLabels
open MoreLabels
open Bdb
open Common
module Unix = UnixLabels

type ptree_settings = { mbar: int;
                        bitquantum: int;
                        treetype: [ `ondisk | `transactional | `inmem ];
                        max_nodes: int;
                        dbdir: string;
                        cache_bytes: int option;
                        pagesize: int option;
                      }

exception No_db

type dbstate = { settings: ptree_settings;
                 dbenv: Dbenv.t;
                 db: Db.t;
               }

(* let num_samples = mbar + 1 *)

(***************************************************************)
(* Database and PTree setup ************************************)
(***************************************************************)

(** DB access methods.  The following will be passed on to the prefixtree *)

let dbstate = ref None

let get_dbs () =
  match !dbstate with
    | None -> raise No_db
    | Some dbs -> dbs

let dbs () = get_dbs ()
let settings () = (get_dbs ()).settings

let closedb () =
  match !dbstate with
      None -> ()
    | Some dbs ->
        Db.close dbs.db;
        Dbenv.close dbs.dbenv

let load key =
  let dbs = get_dbs () in
  let rval = Db.get dbs.db key [] in
  rval

let save txn ~key ~data =
  let dbs = get_dbs () in
  Db.put ?txn dbs.db ~key ~data []

let delete txn key =
  let dbs = get_dbs () in
  Db.del ?txn dbs.db key

(*****************************************************************)
(** txnopt operations do nothing if transactions are not enabled *)

let new_txnopt () =
  let dbs = get_dbs () in
  if dbs.settings.treetype = `transactional then
    Some (Txn.txn_begin dbs.dbenv None [])
  else None

let commit_txnopt txn =
  (match txn with None -> () | Some txn -> Txn.commit txn [])

let abort_txnopt txn =
  (match txn with None -> () | Some txn -> Txn.abort txn)

let checkpoint ?(kbyte=0) ?(min=0) () =
  match !dbstate with
      None -> ()
    | Some dbs ->
        if dbs.settings.treetype = `transactional then (
          plerror 5 "Checkpointing database";
          Txn.checkpoint dbs.dbenv ~kbyte ~min [];
        )

(*****************************************************************)
(** Returns a tuple containing database information needed by ptree *)
let get_db () = match !dbstate with
    None -> None
  | Some dbs ->
      Some (load,save,delete,
            (new_txnopt,commit_txnopt,abort_txnopt),
            dbs.settings.max_nodes)

(*****************************************************************)

(** Set up ptree database if such is necessary *)
let open_ptree_db settings =
  match settings.treetype with

    | `inmem -> None

    | `ondisk | `transactional as treetype ->
        plerror 3 "Opening PTree database";

        if not (Sys.file_exists settings.dbdir )
        then (
          Unix.mkdir settings.dbdir 0o700;
          Utils.initdbconf !Settings.basedir settings.dbdir;
          );

    let dbenv = Dbenv.create () in
    ( match settings.cache_bytes with None -> ()
        | Some cache_bytes -> Dbenv.set_cachesize dbenv
        ~gbytes:0 ~bytes:cache_bytes ~ncache:0);
    Dbenv.dopen dbenv settings.dbdir
      ([Dbenv.INIT_MPOOL; (*Dbenv.INIT_LOCK;*) Dbenv.CREATE] @ (
         match treetype with
           | `transactional -> [Dbenv.INIT_TXN; Dbenv.RECOVER]
           | `ondisk -> []))
      0o600;
    let db = Db.create ~dbenv [] in
    ( match settings.pagesize with
        | None -> ()
        | Some pagesize -> Db.set_pagesize db pagesize );
    Db.dopen db "ptree" Db.BTREE
      ( match treetype with
          | `transactional -> [Db.CREATE; Db.AUTO_COMMIT]
          | `ondisk -> [Db.CREATE] )
      0o600;
    Some { settings = settings;
           dbenv = dbenv;
           db = db;
         }

let init_db settings =
  match !dbstate with
      Some _ -> failwith "Attempt to re-initialize PTreeDB";
    | None -> dbstate := open_ptree_db settings

(** Code for initiating in-memory ptree that reflects on-disk version *)

module PTree = PrefixTree

exception No_ptree

let ptree_ref = ref None

let get_ptree () = match !ptree_ref with
  | None -> raise No_ptree
  | Some ptree -> ptree

(** Setup prefix tree, using disk-based access and transactions
  as specified *)
let init_ptree settings =
  plerror 3 "Setting up PTree data structure";
  let txn = new_txnopt () in
  try
    let db = get_db () in
    let ptree =
      PTree.create ?db
        ~txn ~num_samples:(settings.mbar + 1) ~bitquantum:settings.bitquantum
        ~thresh:(settings.mbar * !Settings.ptree_thresh_mult)
        ()
    in
    commit_txnopt txn;
    plerror 3 "PTree setup complete";
    ptree_ref := Some ptree
  with
      e ->
        abort_txnopt txn;
        closedb ();
        raise e



