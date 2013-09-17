(***********************************************************************)
(* clean_keydb.ml - Executable: Cleans up various problems that occur  *)
(*                  in key databases                                   *)
(*                                                                     *)
(* Currently, this includes:                                           *)
(* - Merging all mergeable keys                                        *)
(* - Eliminating keys with unparseable packet sequences                *)
(* - Eliminating duplicates                                            *)
(*   (Note, this doesn't get rid of ALL duplicates, for instance, if   *)
(*    the same signature is used to sign two different keys, it is not *)
(*    removed. Removal is only done if it leaves a reasonable packet   *)
(*    structure in place.)                                             *)
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

module F(M:sig end) =
struct
  open StdLabels
  open MoreLabels
  open Printf
  open Arg
  open Common
  module Set = PSet.Set
  module Map = PMap.Map
  module Unix = UnixLabels
  open Packet
  open Bdb

  let settings = {
    Keydb.withtxn = !Settings.transactions;
    Keydb.cache_bytes = !Settings.cache_bytes;
    Keydb.pagesize = !Settings.pagesize;
    Keydb.keyid_pagesize = !Settings.keyid_pagesize;
    Keydb.meta_pagesize = !Settings.meta_pagesize;
    Keydb.subkeyid_pagesize = !Settings.subkeyid_pagesize;
    Keydb.time_pagesize = !Settings.time_pagesize;
    Keydb.tqueue_pagesize = !Settings.tqueue_pagesize;
    Keydb.word_pagesize = !Settings.word_pagesize;
    Keydb.dbdir = Lazy.force Settings.dbdir;
    Keydb.dumpdir = Lazy.force Settings.dumpdir;
  }

  (** we need full keydb access because we're playing directly with
    databases and cursors and such
  *)
  module Keydb = Keydb.Unsafe


  let ( |= ) map key = Map.find key map
  let ( |< ) map (key,data) = Map.add ~key ~data map

  let ctr = ref 0
  let tick () =
    incr ctr;
    if !ctr mod 10000 = 0 then
      perror "%d thousand steps processed" (!ctr/1000)


  type action = Delete of key | Swap of (key * key)

  let do_action action = match action with
    | Swap (key1,key2) -> Keydb.swap_keys key1 key2
    | Delete key -> Keydb.delete_key key

  let do_opt f opt = match opt with
    | None -> ()
    | Some x -> f x

  (** Canonicalize a key if it is required.  This assumes that the given
    key is actually in the database *)
  let canonicalize_key key =
    try
      let ckey = Fixkey.canonicalize key in
      if KeyHash.hash ckey <> KeyHash.hash key then
        begin
          perror "Swap found: %s -> %s"
            (KeyHash.hexify (KeyHash.hash key))
            (KeyHash.hexify (KeyHash.hash ckey));
          Some (Swap (key,ckey))
        end
      else None
    with
        Fixkey.Bad_key ->
          perror "Key to be deleted: %s" (KeyHash.hexify (KeyHash.hash key));
          Some (Delete key)


  let at_once = match !Settings.n with
      0 -> 10000
    | n -> n

  let canonicalize_indirect () =
    ctr := 0;
    perror "Starting indirect canonicalization";

    let dbs = Keydb.get_dbs () in
    let filearray = dbs.Keydb.dump.Keydb.filearray in

    let actions = ref [] in
    let num_actions = ref 0 in

    let filter_actions actions =
      let actions = List.map actions
                      ~f:(function
                            | Delete key as action ->
                                (KeyHash.hash key, action)
                            | Swap (key1,key2) as action ->
                                (KeyHash.hash key1, action)
                         )
      in
      let actions = List.sort ~cmp:compare actions in
      let actions = List.filter actions
                      ~f:(fun (hash,action) -> Keydb.has_hash hash)
      in
      List.map ~f:(fun (hash,action) -> action) actions
    in

    let run_stored_actions () =
      let filt_actions = filter_actions !actions in
      perror "doing %d out of %d update actions"
        (List.length filt_actions) (List.length !actions);
      let dbactions =
        List.fold_left ~init:[] filt_actions
          ~f:(fun list action -> match action with
                  Delete key ->
                    (Keydb.key_to_metadata key, Keydb.DeleteKey)::list
                | Swap (key1,key2) ->
                    (Keydb.key_to_metadata key1, Keydb.DeleteKey)::
                    (Keydb.key_to_metadata key2, Keydb.AddKey)::list
             )
      in
      Keydb.apply_md_updates (Array.of_list dbactions);
      Keydb.unconditional_checkpoint ();
      actions := [];
      num_actions := 0
    in

    let add_action action =
      actions := action::!actions;
      incr num_actions;

      if !num_actions >= at_once then
        run_stored_actions ()

    in


    Array.iteri filearray
      ~f:(fun i inchan ->
            perror "Starting keydump %d" i;
            seek_in inchan 0;
            let cin = new Channel.sys_in_channel inchan in
            let get = Key.get_of_channel cin in
            try
              while true do
                tick ();
                let key = get () in
                let action = canonicalize_key key in
                do_opt add_action action
              done
            with
                Not_found -> ()
         );

    run_stored_actions ();
    perror "Indirect canonicalization complete"


  (** iterate through the entire database, replacing all non-canonical keys
    with canonicalized versions.  Delete all non-canonicalizable keys.  Only
    work on keys stored directly in the database.  Keys stored indirectly
    will be fixed by scanning the initial keydump.

    Note that this is not nearly so highly-optimized as canonicalize_indirect.
    However, for most keyservers, most of the keys will be in the indirect
    keydump anyway.
  *)
  let canonicalize_direct () =
    ctr := 0;
    perror "Starting direct canonicalization";
    let clean ~hash ~keystr =
      let skey = Keydb.skey_of_string keystr in
      if not (Keydb.skey_is_offset skey) then
        let key = Keydb.key_of_skey skey in
        tick ();
        (* ignore offsets, they're handled elsewhere *)
        do_opt do_action (canonicalize_key key)
    in
    Keydb.raw_iter clean;
    perror "Direct canonicalization complete"

  let canonicalize () =
    canonicalize_indirect ();
    canonicalize_direct ()

  (***************************************************************)
  (***************************************************************)
  (***************************************************************)

  (** internal function: retrieves list of (key,data) duplicates for a given
    cursor *)
  let rec get_dups_rec cursor accum =
    try
      let (key,data) = Cursor.get cursor Cursor.NEXT_DUP [] in
      get_dups_rec cursor ((key,data)::accum)
    with
        Not_found -> accum

  (** returns pair of key and duplicate data for the given cursor *)
  let get_dups cursor =
    let pairs = get_dups_rec cursor [] in
    match pairs with
        [] -> failwith "get_dups retrieved empty list"
      | (key,data)::tail ->
          let dtail = List.map tail
                        ~f:(fun (tkey,tdata) -> if tkey <> key
                            then failwith "get_dups retrieved non-duplicate"
                            else tdata
                           )
          in
          (key,data::dtail)

  (** checks if a sorted list has duplicates *)
  let rec has_dups list = match list with
      [] -> false
    | [hd] -> false
    | hd1::hd2::tl ->
        if hd1 = hd2 then true
        else has_dups (hd2::tl)

  (** merges keys given the key hashes.  The [keyid] argument is there just to
    make logging more understandable *)
  let merge_from_hashes keyid hashes =
    (* Sort hashes and remove duplicates, if any *)
    let hashes = List.sort ~cmp:compare hashes in
    let hashes =
      if has_dups hashes then (
        perror "Duplicates found in hash list";
        MList.dedup hashes
      ) else hashes
    in

    (** fetches a key from its hash *)
    let key_from_hash hash =
      try
        let key = Keydb.get_by_hash hash in
        let newhash = KeyHash.hash key in
        if newhash <> hash then
          perror "Key hashes do not match up:\n\trequested: %s\n\tfound: %s"
                     (KeyHash.hexify hash) (KeyHash.hexify newhash);
        Some key
      with
          Not_found ->
            perror "Database corruption: Key matched up to keyid not found in database:\n\tkeyid: %s\n\thash: %s"
            (Fingerprint.keyid_to_string keyid) (KeyHash.hexify hash);
            None
    in
    let keys = strip_opt (List.map ~f:key_from_hash hashes) in
    (* compute the list of replacements and apply them *)
    let replacements = Fixkey.compute_merge_replacements keys in
    if List.length replacements > 0
    then perror "%d replacements found" (List.length replacements);
    List.iter replacements
      ~f:(fun (delete_list,newkey) ->
            perror "replacing %d keys with single merged key"
            (List.length delete_list);
            List.iter delete_list
              ~f:(fun key -> perror "removing: %s"
                    (KeyHash.hexify (KeyHash.hash key)));
            perror "adding: %s"
              (KeyHash.hexify (KeyHash.hash newkey));
            Keydb.replace delete_list newkey;
            perror "Transaction complete"
         )





  (** find all sets of key with the same keyid and merge them if possible *)
  let merge () =
    ctr := 0;

    perror "Starting key merge";
    let dbs = Keydb.get_dbs () in
    let c = Cursor.create dbs.Keydb.keyid in

    let (first_keyid,first_hash) = Cursor.get c Cursor.FIRST [] in

    let finished = ref false
    and keyid = ref first_keyid
    and hash = ref first_hash
    in
    while not !finished do
      tick ();
      if Cursor.count c > 1 then (
        let (dup_keyid,hashes) = get_dups c in
        if dup_keyid <> !keyid then failwith "Failure retrieving duplicates";
        let hashes = !hash::hashes in
        perror "%s" ("Multiple keys found with same ID.  " ^
                     "merge_from_hashes called");
        List.iter hashes
          ~f:(fun hash -> perror "Hash: %s" (KeyHash.hexify hash));
        merge_from_hashes !keyid hashes
      );
      try
        let (new_keyid,new_hash) = Cursor.get c Cursor.NEXT [] in
        keyid := new_keyid;
        hash := new_hash
      with
          Not_found -> finished := true
    done;
    perror "Completed key merge"


  (** Run filters that are not already contained in [applied_filters] *)
  let run applied_filters =

    (* only do canonicalize if it's necessary *)
    if not (List.mem "yminsky.dedup" applied_filters) then (
      perror "Deduping keys in database";
      canonicalize ();
      Keydb.set_meta ~key:"filters" ~data:"yminsky.dedup";
      Keydb.unconditional_checkpoint ();
    ) else perror "Database already deduped";


    (* note: if dedup was done, merge should be done again *)
    if not (List.mem "yminsky.dedup" applied_filters)
      || not (List.mem "yminsky.merge" applied_filters)
    then (
      perror "Merging keys in database";
      merge ();
      Keydb.set_meta ~key:"filters" ~data:"yminsky.dedup,yminsky.merge";
      Keydb.unconditional_checkpoint ();
    )
    else perror "Database already merged"



  let comma = Str.regexp ","

  let run () =
    set_logfile "clean";
        perror "Running SKS %s%s" Common.version Common.version_suffix;
    Keydb.open_dbs settings;
    perror "Keydb opened";

    let applied_filters =
      try Str.split comma (Keydb.get_meta "filters")
      with Not_found -> []
    in
    run applied_filters;

    Keydb.close_dbs ()

end
