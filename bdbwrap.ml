(***********************************************************************)
(* bdbwrap.ml - Wrapper module for Bdb to allow for logging of         *)
(*              database operations                                    *)
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

open Common
open Printf

exception Key_exists = Bdb.Key_exists

let wrap name f =
  plerror 10 "( Starting %s" name;
  try
    let rval = f () in
    plerror 10 "  %s Done )" name;
    rval
  with
      e ->
        plerror 10 "  %s Done <%s>)" name (Printexc.to_string e);
        raise e


module Dbenv =
struct
  include Bdb.Dbenv
  let create x = wrap "Dbenv.create" (fun () -> create x)
  let dopen x y z w = wrap "Dbenv.dopen" (fun () -> dopen x y z w)
  let sopen x y z = wrap "Dbenv.sopen" (fun () -> sopen x y z)
  let close x = wrap "Dbenv.close" (fun () -> close x)
  let set_verbose_internal x y z =
    wrap "Dbenv.set_verbose_internal" (fun () -> set_verbose_internal x y z)
  let set_verbose x y z = wrap "Dbenv.set_verbose"
                            (fun () -> set_verbose x y z)
  let set_cachesize x ~gbytes ~bytes ~ncache =
    wrap "Dbenv.set_cachesize" (fun () -> set_cachesize x
                                  ~gbytes ~bytes ~ncache)
end


module Db =
struct
  include Bdb.Db

  let create ?dbenv y =  wrap "Db.create" (fun () -> create ?dbenv y)
  let dopen x y z w u =  wrap "Db.dopen" (fun () -> dopen x y z w u)
  let close x = wrap "Db.close" (fun () -> close x)
  let del x ?txn y = wrap "Db.del" (fun () -> del x ?txn y)
  let put x ?txn ~key ~data y = wrap "Db.put"
                                  (fun () -> put x ?txn ~key ~data y)
  let get x ?txn y z = wrap "Db.get" (fun () -> get x ?txn y z )
  let set_flags x y = wrap "Db.set_flags" (fun () -> set_flags x y)
  let sopen ?dbenv x y ?moreflags z w =
    wrap "Db.sopen" (fun () -> sopen ?dbenv x y ?moreflags z w )

  let set_h_ffactor x y = wrap "Db.set_h_ffactor"
                            (fun () -> set_h_ffactor x y)
  let set_pagesize x y = wrap "Db.set_pagesize" (fun () -> set_pagesize x y)
  let set_cachesize x ~gbytes ~bytes ~ncache =
    wrap "Db.set_cachesize" (fun () -> set_cachesize x ~gbytes ~bytes ~ncache)
  let sync x = wrap "Db.sync" (fun () -> sync x)
end


module Cursor =
struct
  include Bdb.Cursor

  let create ?writecursor ?txn x =
    wrap "Cursor.create" (fun () -> create ?writecursor ?txn x)
  let close x = wrap "Cursor.close" (fun () -> close x)
  let put x y z = wrap "Cursor.put" (fun () -> put x y z)
  let kput x ~key ~data y = wrap "Cursor.kput"
                              (fun () -> kput x ~key ~data y )
  let init x y z  = wrap "Cursor.init" (fun () -> init x y z )
  let init_range x y z  = wrap "Cursor.init_range"
                            (fun () -> init_range x y z )
  let init_both x ~key ~data y =
    wrap "Cursor.init_both" (fun () -> init_both x ~key ~data y)
  let get x y z  = wrap "Cursor.get" (fun () -> get x y z )
  let get_keyonly x y z  = wrap "Cursor.get_keyonly"
                             (fun () -> get_keyonly x y z )
  let del x = wrap "Cursor.del" (fun () -> del x)
  let count x = wrap "Cursor.count" (fun () -> count x )
  let dup ?keep_position x = wrap "Cursor.dup"
                               (fun () -> dup ?keep_position x)
  let ajoin ?nosort x y z = wrap "Cursor.ajoin"
                              (fun () -> ajoin ?nosort x y z)
  let join ?nosort x y z = wrap "Cursor.join"
                             (fun () -> join ?nosort x y z)
end


module Txn =
struct
  include Bdb.Txn
  let set_txn_max x y = wrap "Txn.set_txn_max" (fun () -> set_txn_max x y)
  let abort x = wrap "Txn.abort" (fun () -> abort x)
  let txn_begin x y z = wrap "Txn.txn_begin" (fun () -> txn_begin x y z)
  let checkpoint x ~kbyte ~min y =
    wrap "Txn.checkpoint" (fun () -> checkpoint x ~kbyte ~min y)
  let commit x y = wrap "Txn.commit" (fun () -> commit x y)
end


