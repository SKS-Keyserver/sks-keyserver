/*****************************************************************/
/**  DBENV *******************************************************/
/*****************************************************************/

struct camldbenv {
  DB_ENV *dbenv;
  int closed;
};

/*****************************************************************/
/***  DB  ********************************************************/
/*****************************************************************/

struct camldb {
  DB *db;
  int closed;
};

/*****************************************************************/
/***  DB_CURSOR  *************************************************/
/*****************************************************************/

struct camlcursor {
  DBC *cursor;
  int closed;
};

/*****************************************************************/
/***  DB_TXN  ****************************************************/
/*****************************************************************/

struct camltxn {
  DB_TXN *txn;
  int closed;
};

/*****************************************************************/
/**  DB and DBENV macros  ****************************************/
/*****************************************************************/

// datatype syzes
#define Camldbenv_wosize (sizeof(struct camldbenv))
#define Camldb_wosize (sizeof(struct camldb))
#define Camlcursor_wosize (sizeof(struct camlcursor))
#define Camltxn_wosize (sizeof(struct camltxn))

// Unwrapping functions
#define UW_dbenv(v) (((struct camldbenv *)Data_custom_val(v))->dbenv)
#define UW_dbenv_closed(v) (((struct camldbenv *)Data_custom_val(v))->closed)

#define UW_db(v) (((struct camldb *)Data_custom_val(v))->db)
#define UW_db_closed(v) (((struct camldb *)Data_custom_val(v))->closed)

#define UW_cursor(v) (((struct camlcursor *)Data_custom_val(v))->cursor)
#define UW_cursor_closed(v) (((struct camlcursor *)Data_custom_val(v))->closed)

#define UW_txn(v) (((struct camltxn *)Data_custom_val(v))->txn)
#define UW_txn_closed(v) (((struct camltxn *)Data_custom_val(v))->closed)

#define Is_string(v)   (Is_block(v) && (Tag_val(v) == String_tag))
#define Is_None(v)  (!Is_block(v))
#define Is_Some(v)  (Is_block(v))
#define Some_val(v) (Field(v,0))
#define Flag_val(vflag,flags) (flags[Long_val(vflag)])
