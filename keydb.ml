(***********************************************************************)
(* keydb.ml - Interface for dealing with underlying key database       *)
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
open Printf
open Common
module Set = PSet.Set

(** Invariants to check:

  - All cursors that are created are deleted, even in the case of exceptions
  - All transactions are either committed xor aborted
  - transaction-protected operations are aborted when exceptions occur.
  - Keys are atomically added to and removed from word, key,
    and keyid databases.  Appropriate updates to time db are also made
    atomically.
*)

open Bdb
open Packet

type dbsettings = { withtxn: bool;
                    cache_bytes: int option;
                    pagesize: int option;
                    keyid_pagesize: int option;
                    meta_pagesize: int option;
                    subkeyid_pagesize: int option;
                    time_pagesize: int option;
                    tqueue_pagesize: int option;
                    word_pagesize: int option;
                    dbdir: string;
                    dumpdir: string;
                  }


module type RestrictedKeydb =
sig
  type txn

  val open_dbs : dbsettings -> unit
  val close_dbs : unit -> unit
  val sync : unit -> unit
  val txn_begin : ?parent:txn -> unit -> txn option
  val txn_commit : txn option -> unit
  val txn_abort : txn option -> unit
  val checkpoint : unit -> unit
  val unconditional_checkpoint : unit -> unit

  (* val extract_words : string -> string list *)

  (** access methods *)
  val get_num_keys : unit -> int
  val get_dump_filearray : unit -> in_channel array
  val get_by_words :  max:int -> string list -> key list
  val get_by_hash : string -> key
  val get_keystring_by_hash : string -> string
  val iter : f:(hash:string -> key:key -> 'a) -> unit
  val keyiter : f:(string -> 'a) -> unit
  val get_by_short_subkeyid : string -> key list
  val logquery : ?maxsize:int -> float -> (float * Common.event) list
  val reverse_logquery : ?maxsize:int -> float -> (float * Common.event) list
  val create_hashstream : unit -> string SStream.sstream * (unit -> unit)
  val create_hash_skey_stream :
    unit -> (string * string) SStream.sstream * (unit -> unit)
  val last_ts : unit -> float
  val enqueue_key : txn:txn option -> key -> unit
  val dequeue_key : txn:txn option -> float * key

  type 'a offset = { fnum : int; pos : 'a; }
  and skey =
      KeyString of string
    | Key of Packet.packet list
    | Offset of int offset
    | LargeOffset of int64 offset
  type key_metadata = {
    md_hash : string;
    md_words : string list;
    md_keyid : string;
    md_subkey_keyids : string list;
    md_time : float;
    md_skey : skey;
  }
  val key_to_metadata : ?hash:Digest.t -> key -> key_metadata
  val key_to_metadata_large_offset :
    int64 offset -> Packet.packet list -> key_metadata
  val add_mds : key_metadata list -> unit
  val add_key : ?parent:txn -> ?hash:Digest.t -> Packet.packet list -> unit
  val add_keys : Packet.packet list list -> unit
  val add_key_merge : newkey:bool -> Packet.packet list -> unit
  val add_keys_merge : Packet.packet list list -> unit
  val swap_keys : Packet.packet list -> Packet.packet list -> unit
  val get_meta : string -> string
  val set_meta : key:string -> data:string -> unit
  val replace : Packet.packet list list -> Packet.packet list -> unit
  val delete_key : ?hash:'a -> Packet.packet list -> unit
end


module Unsafe =
struct
  type txn = Bdb.txn

  let word_db_name = "word"
  let key_db_name = "key"
  let keyid_db_name = "keyid"
  let subkey_keyid_db_name = "subkeyid"
  let time_db_name = "time"
  let tqueue_db_name = "tqueue"
  let meta_db_name = "meta"

  let max_internal_matches = !Settings.max_internal_matches

  (**********************************************************)
  (*  Types  ************************************************)
  (**********************************************************)

  type action = DeleteKey | AddKey

  type 'a offset = { fnum: int; pos: 'a; }

  (** Stored key.  Can have a number of formats.
    Eventually this may include death certificates
  *)
  type skey =
    | KeyString of string
    | Key of packet list
    | Offset of int offset
    | LargeOffset of int64 offset

  type dbdump =
      { directory: string;
        filearray: in_channel array;
      }

  type dbstate =
      { settings: dbsettings;
        dbenv: Dbenv.t;
        key: Db.t;
        word: Db.t;
        keyid: Db.t;
        subkey_keyid: Db.t;
        time: Db.t;
        tqueue: Db.t; (** queue of hashes that need
                         to be transmitted to other hosts *)
        meta: Db.t; (** Queue contains metadata, including version
                       information and data about what filters
                       have been applied
                    *)
        dump: dbdump; (** info @ dump files where initial
                         keydump is stored *)
      }

  let dbstate = ref None
  exception No_db

  (***********************************************************************)

  let get_dbs () =
    match !dbstate with
        None -> raise No_db
      | Some dbs -> dbs

  let get_dump_filearray () =
    let dbs = get_dbs () in
    dbs.dump.filearray

  (***********************************************************************)
  (*  Key conversions ****************************************************)
  (***********************************************************************)

  let marshal_offset cout offset =
    cout#write_int offset.fnum;
    cout#write_int offset.pos

  let unmarshal_offset cin =
    let fnum = cin#read_int in
    let offset = cin#read_int in
    { fnum = fnum; pos = offset; }

  (***********************************************************************)

  let marshal_large_offset cout offset =
    cout#write_int offset.fnum;
    cout#write_int64 offset.pos

  let unmarshal_large_offset cin =
    let fnum = cin#read_int in
    let offset = cin#read_int64 in
    { fnum = fnum; pos = offset; }


  (***********************************************************************)

  let skey_of_string s =
    let cin = new Channel.string_in_channel s 0 in
    match cin#read_byte with
        0 -> KeyString cin#read_rest
      | 1 -> Offset (unmarshal_offset cin)
      | 2 -> LargeOffset (unmarshal_large_offset cin)
      | _ -> failwith "Unexpected skey type"

  let skey_to_string skey =
    let cout = Channel.new_buffer_outc 0 in
    (match skey with
         KeyString s -> cout#write_byte 0; cout#write_string s
       | Key key -> cout#write_byte 0; Key.write key cout
       | Offset offset -> cout#write_byte 1; marshal_offset cout offset
       | LargeOffset offset -> cout#write_byte 2;
           marshal_large_offset cout offset
    );
    cout#contents

  let skey_is_offset skey = match skey with
    | KeyString _ | Key _ -> false
    | Offset _ | LargeOffset _ -> true

  let keystring_of_offset offset_union =
    let offset = match offset_union with
        `large_offset offset | `offset offset -> offset
    in
    let dbs = get_dbs () in
    if Array.length dbs.dump.filearray  = 0
    then failwith ("Key could not be fetched from offset: " ^
                   "No key dump found");
    if offset.fnum > Array.length dbs.dump.filearray
    then failwith ("Key could not be fetched from offset: " ^
                   "File number exceeds number of dump files");
    let file = dbs.dump.filearray.(offset.fnum) in
    (match offset_union with
       | `large_offset offset -> LargeFile.seek_in file offset.pos;
       | `offset offset -> seek_in file offset.pos);
    let key = Key.get_of_channel (new Channel.sys_in_channel file) () in
    Key.to_string key

  let keystring_of_skey skey = match skey with
    | KeyString s -> s
    | Key key -> Key.to_string key
    | Offset offset -> keystring_of_offset (`offset offset)
    | LargeOffset offset -> keystring_of_offset (`large_offset offset)

  let keystring_of_string string =
    keystring_of_skey (skey_of_string string)

  let key_of_skey skey =
    match skey with
        KeyString s -> Key.of_string s
      | Key key -> key
      | Offset offset ->
          let dbs = get_dbs () in
          if Array.length dbs.dump.filearray  = 0
          then failwith ("Key could not be fetched from offset: " ^
                         "No key dump found");
          if offset.fnum > Array.length dbs.dump.filearray
          then failwith ("Key could not be fetched from offset: " ^
                         "File number exceeds number of dump files");
          let file = dbs.dump.filearray.(offset.fnum) in
          seek_in file offset.pos;
          Key.get_of_channel (new Channel.sys_in_channel file) ()
      | LargeOffset offset ->
          let dbs = get_dbs () in
          if Array.length dbs.dump.filearray  = 0
          then failwith ("Key could not be fetched from offset: " ^
                         "No key dump found");
          if offset.fnum > Array.length dbs.dump.filearray
          then failwith ("Key could not be fetched from offset: " ^
                         "File number exceeds number of dump files");
          let file = dbs.dump.filearray.(offset.fnum) in
          LargeFile.seek_in file offset.pos;
          Key.get_of_channel (new Channel.sys_in_channel file) ()


  let key_to_string key = skey_to_string (Key key)
  let key_of_string s = key_of_skey (skey_of_string s)

  (***********************************************************************)

  (** returns a list of all elements of the specified directory
    with the given suffix *)
  let read_dir_suff dir suff =
    let dh = Unix.opendir dir in
    let run () =
      let dirs = ref [] in
      while
        match (try Some (Unix.readdir dh)
               with End_of_file -> None)
        with
            Some fname ->
              if Filename.check_suffix fname suff
              then dirs := fname::!dirs;
              true
          | None ->
              false
      do () done;
      List.rev !dirs
    in
    protect ~f:run ~finally:(fun () -> Unix.closedir dh)
  (***********************************************************************)

  let kdbopen ?dbenv fname dbtype ?moreflags pagesize flags mode = (
    let db = Db.create ?dbenv [] in
    (match moreflags with
        None -> ()
      | Some flags -> Db.set_flags db flags );
    (match pagesize with
        None -> ()
      | Some pagesize -> Db.set_pagesize db pagesize );
    Db.dopen db fname dbtype flags mode;
    db)

  (** Initialization code for database *)
  let open_dbs settings =
    plerror 3 "Opening KeyDB database";
    match !dbstate with
        Some x -> failwith ("Keydb.open_dbs: Attempt to open when " ^
                            "close_dbs hasn't been called")
      | None ->
          let dbenv =  Dbenv.create () in
          ( match settings.cache_bytes with None -> ()
              | Some cache_bytes -> Dbenv.set_cachesize dbenv
                  ~gbytes:0 ~bytes:cache_bytes ~ncache:0);
          Dbenv.dopen dbenv settings.dbdir
            ( [ Dbenv.INIT_MPOOL; Dbenv.CREATE; (* Dbenv.INIT_LOCK *) ]
              @ ( if settings.withtxn then [ Dbenv.INIT_TXN; Dbenv.RECOVER ]
                  else [] ) )
            0o600;

          let openflags = (if settings.withtxn then [Db.CREATE; Db.AUTO_COMMIT]
                           else [Db.CREATE])
          in
          let key = kdbopen ~dbenv key_db_name Db.BTREE ~moreflags:[]
             settings.pagesize openflags 0o600
          in
          let keyid = kdbopen ~dbenv keyid_db_name Db.BTREE
             ~moreflags:[Db.DUPSORT] settings.keyid_pagesize
              openflags 0o600
          in
          let meta = kdbopen  ~dbenv meta_db_name Db.BTREE
             ~moreflags:[] settings.meta_pagesize openflags 0o600
          in
          let subkey_keyid = kdbopen ~dbenv subkey_keyid_db_name Db.BTREE
             ~moreflags:[Db.DUPSORT] settings.subkeyid_pagesize
              openflags 0o600
          in
          let time = kdbopen ~dbenv time_db_name Db.BTREE
             ~moreflags:[Db.DUPSORT] settings.time_pagesize openflags 0o600
          in
          let tqueue = kdbopen  ~dbenv tqueue_db_name Db.BTREE ~moreflags:[]
             settings.tqueue_pagesize openflags 0o600
          in
          let word = kdbopen ~dbenv word_db_name Db.BTREE
             ~moreflags:[Db.DUPSORT] settings.word_pagesize openflags 0o600
          in

          (** Sets up array of dump files for entries where
            file offset is stored instead of key contents *)
          let dump =
            let dir = settings.dumpdir in
            if (Sys.file_exists dir &&
                (Unix.stat dir).Unix.st_kind = Unix.S_DIR)
            then
              let pgpfiles = read_dir_suff dir ".pgp" in
              let pgpfiles = List.sort ~cmp:compare pgpfiles in
              let pgpfiles =
                List.map ~f:(fun f -> Filename.concat dir f) pgpfiles in
              let pgpfiles = Array.of_list pgpfiles in
              { directory = dir;
                filearray =
                  Array.map
                    ~f:(open_in_gen [Open_rdonly; Open_binary] 0o600)
                    pgpfiles
              }
            else
              { directory = "";
                filearray = Array.make 0 stdin;
              }
          in

          if settings.withtxn then Txn.checkpoint dbenv ~kbyte:0 ~min:0 [];
          dbstate := Some { settings = settings;
                            dbenv = dbenv;
                            word = word;
                            key = key;
                            keyid = keyid;
                            subkey_keyid = subkey_keyid;
                            time = time;
                            dump = dump;
                            meta = meta;
                            tqueue = tqueue;
                          }

  (***********************************************************************)

  let close_dump dbs =
    let files = dbs.dump.filearray in
    Array.iter files ~f:(fun file -> close_in file)

  (***********************************************************************)

  let close_dbs () = match !dbstate with
      None -> raise No_db
    | Some dbs ->
        Db.close dbs.key;
        Db.close dbs.word;
        Db.close dbs.time;
        Db.close dbs.keyid;
        Db.close dbs.subkey_keyid;
        Db.close dbs.tqueue;
        Db.close dbs.meta;
        Dbenv.close dbs.dbenv;
        close_dump dbs;
        dbstate := None

  (***********************************************************************)

  let sync () =
    let dbs = get_dbs () in
    Db.sync dbs.key;
    Db.sync dbs.word;
    Db.sync dbs.time;
    Db.sync dbs.keyid;
    Db.sync dbs.subkey_keyid;
    Db.sync dbs.tqueue;
    Db.sync dbs.meta

  (***********************************************************************)

  let txn_begin ?parent () =
    let dbs = get_dbs () in
    if dbs.settings.withtxn then Some (Txn.txn_begin dbs.dbenv parent [])
    else None

  (***********************************************************************)

  let txn_commit txn = match txn with
      None -> () | Some txn -> Txn.commit txn []

  (***********************************************************************)

  let txn_abort txn = match txn with
      None -> () | Some txn -> Txn.abort txn

  (***********************************************************************)

  let checkpoint () =
    let dbs = get_dbs () in
    if dbs.settings.withtxn then
      Txn.checkpoint dbs.dbenv ~kbyte:(1024 * 5) ~min:0 []

  (***********************************************************************)

  let unconditional_checkpoint () =
    let dbs = get_dbs () in
    if dbs.settings.withtxn then
      Txn.checkpoint dbs.dbenv ~kbyte:0 ~min:0 []


  (***********************************************************************)
  (** Entry preparation code: utilities for formatting data for placement in
    database *)
  (***********************************************************************)

  let float_to_string f =
    let cout = Channel.new_buffer_outc 8 in
    cout#write_float f;
    cout#contents

  let float_of_string s =
    let cin = new Channel.string_in_channel s 0 in
    cin#read_float

  let event_to_string event =
    let cout = Channel.new_buffer_outc 9 in
    ( match event with
          Add hash -> cout#write_byte 0; cout#write_string hash
        | Delete hash -> cout#write_byte 1; cout#write_string hash
    );
    cout#contents

  let event_of_string string =
    let cin = new Channel.string_in_channel string 0 in
    match cin#read_byte with
        0 -> Add cin#read_rest
      | 1 -> Delete cin#read_rest
      | _ -> failwith "Failure parsing event string"

  let flatten_array_of_lists a =
    (** chooses element from lists in a *)
    let rec choose i =
      if i >= Array.length a then raise Not_found
      else
        match a.(i) with
            [] -> choose (i+1)
          | hd::tl -> hd
    in

    let total_length =
      Array.fold_left ~init:0
        ~f:(fun sum list -> sum + List.length list) a
    in
    try
      let newarray = Array.make total_length (choose 0) in

      (* fill newarray  *)
      let ctr = ref 0 in
      Array.iter a
        ~f:(List.iter ~f:(fun el -> newarray.(!ctr) <- el; incr ctr));
      newarray
    with
        Not_found -> [||]

  (***********************************************************************)
  (*  Access methods  ***************************************************)
  (***********************************************************************)


  (** fetch all matches from a joined cursor *)
  let jcursor_get_all ~max c =
    let rec loop max list =
      if max = 0 then list
      else (
        match (try Some (Cursor.get c Cursor.NULL [])
               with Not_found -> None)
        with
            Some (key,data) -> loop (max - 1) (data :: list)
          | None -> list
      )
    in
    loop max []

  (** retrieve keys based on words found in uid strings *)
  let get_by_words ~max wordlist =
    let dbs = get_dbs () in
    try
      let cursors = List.map ~f:(fun word ->
                                   let c = Cursor.create dbs.word in
                                   ignore (Cursor.init c word []);
                                   c )
                      wordlist in
      let run () =
        let lengths = List.map ~f:Cursor.count cursors in
        if MList.min lengths > max_internal_matches
        then raise (Invalid_argument "Insufficiently specific words");
        let keystrings =
          let cj = Cursor.join dbs.key cursors [] in
          protect ~f:(fun () -> jcursor_get_all ~max cj)
            ~finally:(fun () -> Cursor.close cj)
        in
        if List.length keystrings >= max then
          raise (Invalid_argument "Too many responses")
        else
          List.map ~f:key_of_string keystrings
      in
      protect ~f:run ~finally:(fun () -> List.iter cursors ~f:Cursor.close)
    with
        Not_found -> []

  (***********************************************************************)

  let get_skeystring_by_hash hash =
    let dbs = get_dbs () in
    Db.get dbs.key hash []

  let get_keystring_by_hash hash =
    keystring_of_string (get_skeystring_by_hash hash)

  (***********************************************************************)

  (** retrieves key by hash *)
  let get_by_hash hash =
    key_of_string (get_skeystring_by_hash hash)

  (** returns true iff db contains specified hash *)
  let has_hash hash =
    try ignore (get_skeystring_by_hash hash); true
    with Not_found -> false

  (** Verification functions *)

  let check_word_hash_pair ~word ~hash =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.word in
    let run () =
      try
        Cursor.init_both c ~key:word ~data:hash [];
        true
      with
          Not_found -> false
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)

  let check_keyid_hash_pair ~keyid ~hash =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.keyid in
    let run () =
      try
        Cursor.init_both c ~key:keyid ~data:hash [];
        true
      with
          Not_found -> false
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)


  let get_keystrings_by_hashes hashes =
    (* sort to improve performance, although this should
       only really help for very large lists. *)
    let hashes = List.sort ~cmp:compare hashes in
    let keystr_opts =
      List.map ~f:(fun hash ->
                     try Some (get_keystring_by_hash hash)
                     with Not_found -> None)
        hashes
    in
    MList.strip_opt keystr_opts


  (***********************************************************************)

  let keyid_iter ~f =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.keyid in
    let rec loop get_type =
      match (try Some (Cursor.get c get_type []) with Not_found -> None)
      with
        | Some (key,data) ->
            f ~keyid:key ~hash:data;
            loop Cursor.NEXT
        | None -> ()
    in
    protect ~f:(fun () -> loop Cursor.FIRST)
      ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)

  let raw_iter ~f =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.key in
    let rec loop get_type =
      match (try Some (Cursor.get c get_type []) with Not_found -> None)
      with
        | Some (key,data) ->
            f ~hash:key ~keystr:data;
            loop Cursor.NEXT
        | None -> ()
    in
    protect ~f:(fun () -> loop Cursor.FIRST)
      ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)

  let iter ~f =
    raw_iter ~f:(fun ~hash ~keystr ->
                   f ~hash ~key:(key_of_string keystr))

  (***********************************************************************)

  let keyiter ~f =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.key in
    let rec loop get_type =
      match (try Some (Cursor.get_keyonly c get_type [])
             with Not_found -> None)
      with
        | Some key -> f key; loop Cursor.NEXT
        | None -> ()
    in
    protect ~f:(fun () -> loop Cursor.FIRST)
      ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)

  let get_hashes_by_keyid db keyid =
    let c = Cursor.create db in
    let run () =
      let rec loop list =
        match (try Some (Cursor.get c Cursor.NEXT_DUP [])
               with Not_found -> None)
        with
          | Some (key,data) -> loop (data::list)
          | None -> List.rev list
      in
      try
        let first = Cursor.init c keyid [] in
        let hashes = loop [first] in
        hashes
      with
          Not_found -> []
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)


  let get_skeystrings_by_keyid db keyid =
    let hashes = get_hashes_by_keyid db keyid in
    MList.strip_opt
      (List.map ~f:(fun hash ->
                     try Some (get_skeystring_by_hash hash)
                     with Not_found ->
                       plerror 3 "%s %s"
                       "Failed lookup of skeystring from hash"
                       (KeyHash.hexify hash);
                       None
                   )
         hashes)

  (** returns list of keys with a primary key with the given short keyid *)
  let get_by_short_keyid keyid =
    if String.length keyid <> 4
    then failwith (sprintf "wrong keyid length %d" (String.length keyid));
    let dbs = get_dbs () in
    let skeystrings = get_skeystrings_by_keyid dbs.keyid keyid in
    List.map ~f:key_of_string skeystrings

  (** returns list of keys with a primary key or subkey with the given short keyid *)
  let get_by_short_subkeyid keyid =
    if String.length keyid <> 4
    then failwith (sprintf "wrong keyid length %d" (String.length keyid));
    let dbs = get_dbs () in
    let skeystrings =
      get_skeystrings_by_keyid dbs.keyid keyid @
      get_skeystrings_by_keyid dbs.subkey_keyid keyid
    in
    List.map ~f:key_of_string skeystrings

  (** return up to [maxsize] keys strictly after provided timestamp *)
  let logquery ?(maxsize=5000) timestamp =

    let dbs = get_dbs () in
    let c = Cursor.create dbs.time in
    let run () =
      try
        let (timestr,eventstr) =
          Cursor.init_range c (float_to_string timestamp) [] in
        let fst_time = float_of_string timestr in
        let fst_event = event_of_string eventstr in
        assert (fst_time >= timestamp);
        let rec loop count list = match count with
          | 0 -> List.rev list
          | _ ->
              match (try Some (Cursor.get c Cursor.NEXT [])
                     with Not_found -> None)
              with
                  None -> List.rev list
                | Some (time,event) ->
                    let (time,event) = (float_of_string time,
                                               event_of_string event)
                    in
                    loop (count - 1) ((time,event)::list)
        in
        if fst_time = timestamp then loop maxsize []
        else loop (maxsize - 1) [(fst_time,fst_event)]
      with
          Not_found -> []
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)

  (** return up to [maxsize] keys counting back from the end of the
    database, and going no farther back then [timestamp] *)

  let reverse_logquery ?(maxsize=5000) timestamp =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.time in
    let run () =
      try
        let (timestr,eventstr) =
          Cursor.get c Cursor.LAST [] in
        let fst_time = float_of_string timestr in
        let fst_event = event_of_string eventstr in
        if fst_time < timestamp then []
        else
          let rec loop count list = match count with
            | 0 -> list
            | _ ->
                begin
                match (try Some (Cursor.get c Cursor.PREV [])
                       with Not_found -> None)
                with
                    None -> list
                  | Some (time,event) ->
                      let (time,event) = (float_of_string time,
                                          event_of_string event)
                      in
                      if time < timestamp then list
                      else loop (count - 1) ((time,event)::list)
                end
          in
          loop (maxsize - 1) [(fst_time,fst_event)]
      with
          Not_found -> []
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)

  (***********************************************************************)

  let create_hashstream () =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.keyid in
    let first = snd (Cursor.get c Cursor.FIRST []) in
    let close () = Cursor.close c in
    let next () = (try Some (snd (Cursor.get c Cursor.NEXT []))
                   with Not_found -> None) in
    let stream = SStream.make ~first next in
    (stream,close)

  let create_hash_skey_stream () =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.key in
    let first = Cursor.get c Cursor.FIRST [] in
    let close () = Cursor.close c in
    let next () = (try Some (Cursor.get c Cursor.NEXT [])
                   with Not_found -> None) in
    let stream = SStream.make ~first next in
    (stream,close)


  (***********************************************************************)

  let last_ts () =
    let dbs = get_dbs () in
    let c = Cursor.create dbs.time in
    protect ~f:(fun () -> float_of_string (Cursor.get_keyonly c
                                             Cursor.LAST []))
      ~finally:(fun () -> Cursor.close c)


  (**************************************************************)
  (**  Functions for updating key database *)
  (**************************************************************)

  (**********************************************************)

  (** Add key to transmission queue for sending to other
    (non-SKS) keyservers. *)
  let enqueue_key ~txn key =
    let txn =
      match txn with Some txn -> txn
        | None -> failwith "transaction required for Keydb.enqueue_key"
    in
    let dbs = get_dbs () in
    let c = Cursor.create ~txn dbs.tqueue in
    let run () =
      let timestr = float_to_string (Unique_time.get ()) in
      Cursor.kput c ~key:timestr ~data:(key_to_string key) Cursor.KEYLAST
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)

  (** Extract key from transmission queue for receiving from
    (non-SKS) keyservers. *)
  let dequeue_key ~txn =
    let txn = match txn with Some txn -> txn
      | None -> failwith "transaction required for Keydb.dequeue_key"
    in
    let dbs = get_dbs () in
    let c = Cursor.create ~txn dbs.tqueue in
    let run () =
      let (timestr,keystr) = Cursor.get c Cursor.FIRST [] in
      Cursor.del c;
      (float_of_string timestr, key_of_string keystr)
    in
    protect ~f:run ~finally:(fun () -> Cursor.close c)


  (***********************************************************************)

  type key_metadata = { md_hash: string;
                        md_words: string list;
                        md_keyid: string;
                        md_subkey_keyids: string list;
                        md_time: float;
                        md_skey: skey;
                      }

  let shorten_offset offset =
    if offset.pos <= Int64.of_int max_int then
      Offset { fnum = offset.fnum;
               pos = Int64.to_int offset.pos;
             }
    else
      LargeOffset offset

  let key_to_metadata_large_offset offset key =
    let (keyid,subkey_keyids) = Fingerprint.keyids_from_key ~short:true key in
    { md_hash = KeyHash.hash key;
      md_words = Key.to_words key;
      md_keyid = keyid;
      md_subkey_keyids = subkey_keyids;
      md_time = Unique_time.get ();
      md_skey = shorten_offset offset;
    }

  let key_to_metadata_offset offset key =
    let (keyid,subkey_keyids) = Fingerprint.keyids_from_key ~short:true key in
    { md_hash = KeyHash.hash key;
      md_words = Key.to_words key;
      md_keyid = keyid;
      md_subkey_keyids = subkey_keyids;
      md_time = Unique_time.get ();
      md_skey = Offset offset;
    }

  let key_to_metadata ?hash key =
    let (keyid,subkey_keyids) = Fingerprint.keyids_from_key ~short:true key in
    { md_hash = (match hash with
                   | None -> KeyHash.hash key
                   | Some hash -> hash);
      md_words = Key.to_words key;
      md_keyid = keyid;
      md_subkey_keyids = subkey_keyids;
      md_time = Unique_time.get ();
      md_skey = Key key;
    }

  (***********************************************************************)

  (** Bulk addition of key-metadata.  Used by fastbuild, so no transactional
    support required or provided.  *)
  let add_mds mds =

    let dbs = get_dbs () in
    let mds = Array.of_list mds in

    (* Add hash-key mappings *)
    Array.sort mds ~cmp:(fun md1 md2 -> compare md1.md_hash md2.md_hash);
    Array.iter
      ~f:(fun md ->
            try Db.put dbs.key ~key:md.md_hash
              ~data:(skey_to_string md.md_skey)
              [Db.NOOVERWRITE]
            with Key_exists -> ()
         )
      mds;

    let multi_add db getindices =
      let pair_array =
        Array.map
          ~f:(fun md ->
                let indices = getindices md in
                List.rev_map ~f:(fun index -> (index,md.md_hash)) indices)
          mds
      in
      let pairs = flatten_array_of_lists pair_array in
      Array.sort ~cmp:compare pairs;
      Array.iter ~f:(fun (index,hash) ->
                       try Db.put db ~key:index ~data:hash [Db.NODUPDATA]
                       with Key_exists -> ()
                    )
        pairs
    in

    multi_add dbs.word (fun md -> md.md_words);
    multi_add dbs.subkey_keyid (fun md -> md.md_subkey_keyids);
    multi_add dbs.keyid (fun md -> [md.md_keyid]);

    (* Add time-hash mappings.  No sorting required *)
    Array.sort mds ~cmp:(fun md1 md2 -> compare md1.md_time md2.md_time);
    Array.iter mds
      ~f:(fun md ->
            let timestr = float_to_string md.md_time
            and eventstr = event_to_string (Add md.md_hash) in
            Db.put dbs.time ~key:timestr ~data:eventstr [Db.NODUPDATA])


  (****************************************************************)

  let apply_md_updates_txn ~txn updates =
    let dbs = get_dbs () in

    (* action is included in sort, to ensure that deletes get
       processed before additions.  *)
    Array.sort updates ~cmp:(fun (md1,action) (md2,action) ->
                               compare (md1.md_hash,action)
                               (md2.md_hash,action)
                            );

    (* Check for hash duplicates *)
    for i = 0 to Array.length updates - 2 do
      if (fst updates.(i)).md_hash = (fst updates.(i+1)).md_hash
      then failwith ("Keydb.apply_md_updates_txn: duplicate hashes " ^
                     "found in update list")
    done;

    begin
      (* add hash-key mappings to database *)
      let c = Cursor.create ?txn dbs.key in
      let run () =
        Array.iter updates
          ~f:(function
                | (md,AddKey) ->
                    Db.put dbs.key ?txn ~key:md.md_hash
                    ~data:(skey_to_string md.md_skey) [Db.NOOVERWRITE]
                | (md,DeleteKey) ->
                    try
                      ignore (Cursor.init c md.md_hash [] : string);
                      Cursor.del c
                    with Not_found -> ()
             )
      in
      protect ~f:run ~finally:(fun () -> Cursor.close c);
    end;

    (* function for doing multiple updates at once *)
    let multi_update db getindices options =

      let triple_array =
        Array.map updates
          ~f:(fun (md,action) ->
                let indices = getindices md in
                List.rev_map indices
                  ~f:(fun index -> (index,md.md_hash,action))
             )
      in
      let triples = flatten_array_of_lists triple_array in
      Array.sort ~cmp:compare triples;

      let c = Cursor.create ?txn db in
      let run () =
        Array.iter triples
          ~f:(function
                | (index,hash,AddKey) ->
                    Db.put db ?txn ~key:index ~data:hash options
                | (index,hash,DeleteKey) ->
                    try
                      Cursor.init_both c ~key:index ~data:hash [];
                      Cursor.del c
                    with
                        Not_found -> ()
             )
      in
      protect ~f:run ~finally:(fun () -> Cursor.close c);
    in

    multi_update dbs.word (fun md -> md.md_words) [Db.NODUPDATA];
    multi_update dbs.subkey_keyid (fun md -> md.md_subkey_keyids) [];
    multi_update dbs.keyid (fun md -> [md.md_keyid]) [];

    (* Add time-hash mappings.  Note that there are no hash duplicates,
       so the time ordering does not matter *)
    Array.sort updates ~cmp:(fun (md1,action) (md2,action) ->
                               compare md1.md_time md2.md_time);
    Array.iter updates
      ~f:(fun (md,action) ->
            let timestr = float_to_string md.md_time in
            let event = match action with
                AddKey -> Add md.md_hash | DeleteKey -> Delete md.md_hash
            in
            let eventstr = event_to_string event in
            Db.put ?txn dbs.time ~key:timestr ~data:eventstr [Db.NODUPDATA]
         )


  (****************************************************************)

  let apply_md_updates updates =
    let txn = txn_begin () in
    try
      apply_md_updates_txn ~txn updates;
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break
      | e ->
          eplerror 1 e "apply_md_updates failed -- aborting txn";
          txn_abort txn;
          raise e


  (****************************************************************)

  let add_md_txn ?txn md =
    apply_md_updates_txn ~txn [| md,AddKey |]

  (**********************************************************)

  (** add a single key with transaction possibly passed in *)
  let add_key_txn ?txn ?hash key =
    let md = key_to_metadata ?hash key in
    add_md_txn ?txn md

  (**********************************************************)

  (** Does the required transactional wrapping around add_key_txn *)
  let add_key ?parent ?hash key =
    let txn = txn_begin ?parent () in
    try
      add_key_txn ?txn ?hash key;
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break
      | e ->
          eplerror 2 e "Keydb.add_key -- Aborting transaction";
          txn_abort txn;
          raise e

(****************************************************************)

  (** Does transactional wrapping around key adding,
    allowing multiple keys to be added in a single transaction.*)
  let add_multi_keys keys =
    let txn = txn_begin () in
    try
      List.iter
        ~f:(fun key ->
              try
                add_key ?parent:txn key
              with
                | Key_exists ->
                    plerror 2 "%s"
                      ("add_multi_keys: Key_exists. " ^
                       "continuing transaction");
                    let hashstr = KeyHash.hexify (KeyHash.hash key) in
                    plerror 4 "Hash of duplicate key: %s" hashstr
                | e ->
                    eplerror 2 e "%s"
                      ("add_multi_keys: unexpected error.  " ^
                       "Continuing transaction on other keys")
           )
        keys;
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break

      | e ->
          txn_abort txn;
          eplerror 2 e "Keydb.add_multi_key -- Aborting transaction";
          raise e

  (***********************************************************************)

  (** Adds multiple keys at once --- no transactional support *)
  let add_keys keys =
    let mds = List.map ~f:key_to_metadata keys in
    add_mds mds

  (***********************************************************************)

  let key_to_merge_updates key =
    let hash = KeyHash.hash key in
    try
      if has_hash hash then [] else
        let keyid = Fingerprint.keyid_from_key ~short:true key in
        let potential_merges = List.filter ~f:(fun x -> x <> key)
                                 (get_by_short_keyid keyid)
        in
        plerror 4 "%d potential merges found for keyid %s"
          (List.length potential_merges) (KeyHash.hexify keyid);
        let (deletions,mergedkey) =
          List.fold_left ~init:([],key) potential_merges
            ~f:(fun (updates,key) x ->
                  match KeyMerge.merge key x with
                    | None -> (updates,key)
                    | Some mergedkey ->
                        ((x, DeleteKey)::updates,
                         mergedkey)
               )
        in
        let addition = (mergedkey,AddKey) in
        let updates = addition::deletions in
        let updates = List.rev updates in
        let updates = List.map updates
                        ~f:(fun (key,action) -> (key_to_metadata key,action))
        in
        plerror 4 "%d updates found before filtering" (List.length updates);
        updates
    with
      | Sys.Break | Eventloop.SigAlarm as e -> raise e
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break
      | e ->
          eplerror 2 e "Keydb.key_to_merge_updates: error in key %s"
            (KeyHash.hexify hash);
          []


  (**********************************************************)

  let sort_remove updates =
    let updates = List.stable_sort updates
                    ~cmp:(fun (md1,action) (md2,action) ->
                            compare md1.md_hash md2.md_hash)
    in
    let rec clean updates list = match updates with
      | [] -> List.rev list
      | [el] -> clean [] (el::list)
      | (md1,action1)::(md2,action2)::tl ->
          if md1.md_hash = md2.md_hash &&
            (action1 = DeleteKey && action2 = AddKey
             || action2 = DeleteKey && action1 = AddKey
            )
          then clean tl list
          else clean ((md2,action2)::tl) ((md1,action1)::list)
    in
    clean updates []

  (**********************************************************)

  let add_keys_merge_txn ~txn keys =
    let updates = List.map ~f:key_to_merge_updates keys in
    let updates = List.concat updates in
    let updates = sort_remove updates in
    plerror 3 "Applying %d changes" (List.length updates);
    List.iter updates
      ~f:(function
            | (md,AddKey) ->
                plerror 3 "Adding hash %s" (KeyHash.hexify md.md_hash)
            | (md,DeleteKey) ->
                plerror 3 "Del'ng hash %s" (KeyHash.hexify md.md_hash)
         );
    apply_md_updates_txn ~txn (Array.of_list updates);
    List.length updates

  (**********************************************************)

  let add_keys_merge keys =
    let txn = txn_begin () in
    try
      ignore (add_keys_merge_txn ~txn keys);
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break

      | e ->
          eplerror 1 e "add_keys_merge failed";
          txn_abort txn;
          raise e

  (**********************************************************)

  let add_key_merge ~newkey key =
    let txn = txn_begin () in
    try
      let number_of_updates = add_keys_merge_txn ~txn [key] in
      if newkey && number_of_updates > 0 then (
        plerror 4 "%s" ("Keydb.add_key_merge: Enqueing new key " ^
                        "for transmission to other hosts");
        enqueue_key ~txn key
      );
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break

      | e ->
          txn_abort txn;
          raise e

  (**********************************************************)

  let delete_key_txn ?txn ?hash key =
    let md = key_to_metadata ?hash key in
    apply_md_updates_txn ~txn [| md,DeleteKey |]

  (***********************************************************************)

  (** replace [key1] with [key2] in the database *)
  let swap_keys key1 key2 =
    let txn = txn_begin () in
    try
      delete_key_txn ?txn key1;
      add_key_txn ?txn key2;
      (match txn with None  -> () | Some txn -> Txn.commit txn [])
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break

      | e ->
          eplerror 2 e "Keydb.swap_keys -- Aborting transaction";
          txn_abort txn;
          raise e


  (**********************************************************)

  let delete_key ?hash key =
    let txn = txn_begin () in
    try
      delete_key_txn ?txn key;
      (match txn with None  -> () | Some txn -> Txn.commit txn [])
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break

      | e ->
          txn_abort txn;
          eplerror 2 e "Keydb.delete_key -- Aborting transaction";
          raise e


  (**********************************************************)

  (** Operations on metadata *)

  let get_meta key =
    let dbs = get_dbs () in
    Db.get dbs.meta key []

  let set_meta_txn ~txn ~key ~data =
    let dbs = get_dbs () in
    Db.put ?txn dbs.meta ~key ~data []

  let set_meta ~key ~data =
    let txn = txn_begin () in
    try
      set_meta_txn ~txn ~key ~data;
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break
      | e ->
          txn_abort txn;
          raise e

  (**********************************************************)

  (** atomically remove all keys on [delete_list] and add key [newkey] *)
  let replace delete_list newkey =
    let txn = txn_begin () in
    try
      let newkey_update = (key_to_metadata newkey, AddKey) in
      let delete_updates =
        List.map ~f:(fun key -> (key_to_metadata key,DeleteKey)) delete_list in
      apply_md_updates_txn ~txn (Array.of_list (sort_remove (newkey_update::delete_updates)));
      txn_commit txn
    with
      | Bdb.DBError s as e ->
          eplerror 0 e "Fatal database error";
          raise Sys.Break
      | e ->
          txn_abort txn;
          raise e


  let get_num_keys () =
    let ctr = ref 0 in
    keyid_iter ~f:(fun ~keyid ~hash -> incr ctr);
    !ctr

end


module Safe = (Unsafe : RestrictedKeydb)
