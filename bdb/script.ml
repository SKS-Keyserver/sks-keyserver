open Db3
open Printf

(* let _ = popt (Some 8)
   let _ = popt None     *)

(* let _ = Dbenv.sopen dbe "DBTEST"
   [Dbenv.DB_CREATE ; Dbenv.DB_INIT_MPOOL] 0o777 *)

let db = Db.sopen "testdb" Db.BTREE [Db.CREATE] 0o777
let _ =
  (try
     let rval = Db.get db "foobar" [] in
     printf "Result unexpectedly found: %s\n" rval
   with
       Not_found -> printf "Not_found\n");
  Db.put db ~key:"foo" ~data:"bar" [];
  let data = Db.get db "foo" [] in
  printf "key: %s, data: %s\n" "foo" data;
  Db.del db "foo";
  (try
     let rval = Db.get db "foobar" [] in
     printf "Result unexpectedly found: %s\n" rval
   with
       Not_found -> printf "Not_found\n")


