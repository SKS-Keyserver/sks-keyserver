/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Francois Rouaix, projet Cristal, INRIA Rocquencourt      */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#define Max_dballoc 1000000


struct camldbenv {
  final_fun f;
  DBENV *dbenv;
  int closed;
}

#define Camldbenv_wosize \
  ((sizeof(struct camldbenv) + sizeof(value) - 1) / sizeof(value))

#define Camldbenv_dbenv(v) (((struct camldbenv *)(Bp_val(v)))->dbenv)
#define Camldbenv_closed(v) (((struct camldbenv *)(Bp_val(v)))->closed)

#define Is_string(v)   (Is_block(v) && (Tag_val(v) == String_tag))


/* A DB is a finalized value containing
 *  a pointer to the DB,
 *  a pointer to the openstruct
 *    (this could be removed if we were sure that the library doesn't keep
 *     a pointer to it !)
 */
struct camldb {
  final_fun f;
  DB *db;
  // BTREEINFO *info;
  int closed;
};


#define Camldb_wosize \
  ((sizeof(struct camldb) + sizeof(value) - 1) / sizeof(value))

#define Camldb_db(v) (((struct camldb *)(Bp_val(v)))->db)
#define Camldb_closed(v) (((struct camldb *)(Bp_val(v)))->closed)

#define Is_string(v)   (Is_block(v) && (Tag_val(v) == String_tag))
