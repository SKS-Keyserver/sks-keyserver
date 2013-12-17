(***********************************************************************)
(* dbserver.ml - Executable: server process that handles database and  *)
(*               database queries.                                     *)
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
  open Common
  open Packet
  module Unix = UnixLabels
  open Unix
  open DbMessages
  open Request
  open Pstyle
  open Sys

  let () =
    set_logfile "db";
    plerror 0 "sks_db, SKS version %s%s" version version_suffix;
    plerror 0 "Using BerkelyDB version %s" (Bdb.version(););
    plerror 0 "Copyright Yaron Minsky 2002, 2003, 2004";
    plerror 0 "Licensed under GPL. See LICENSE file for details";
    plerror 3 "http port: %d" http_port

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
  module Keydb = Keydb.Safe

  (* Simple server code for handling DB requests.  This is the main control
     code for the DB. *)

  let withtxn = !Settings.transactions
  let dbdir = Lazy.force Settings.dbdir
  let () =
    if not withtxn then
      failwith "Running sks_db without transactions is no longer supported."

  let websocks =
    List.rev_map ~f:Eventloop.maybe_create_sock
      ((if !Settings.use_port_80 then make_addr_list http_address 80 else [])
       @ make_addr_list http_address http_port)
  let websocks =
    List.fold_right ~init:[]
      ~f:(function
	   | Some sock -> fun acc -> sock :: acc
	   | None -> fun acc -> acc)
      websocks
  let () =
    if websocks = [] then
      failwith "Could not listen on any address."

  let () =
    if Sys.file_exists db_command_name
    then Unix.unlink db_command_name
  let comsock = Eventloop.create_sock db_command_addr


  (*********************************************************************)
  (** Database checkpointing and syncing *)

  let sync () =
    perror "Syncing database";
    Keydb.sync ();
    perror "Syncing complete"

  let sync_interval = !Settings.db_sync_interval

  let checkpoint () =
    perror "Checkpointing database";
    Keydb.checkpoint ();
    perror "Checkpointing complete"

  let checkpoint_interval = !Settings.checkpoint_interval

  (***************************************************************)
  (*  Helper functions for http request handler   ****************)
  (***************************************************************)

  let ascending = compare
  let descending x y = compare y x

  (** sorts keys by time, dropping keys with no time *)
  let tsort_keys keys =
    let kpairs =
      List.fold_left ~init:[] keys
        ~f:(fun list key ->
              try
                let ki = ParsePGP.parse_pubkey_info (List.hd key) in
                (ki.pk_ctime,key)::list
              with
                | Sys.Break as e -> raise e
                | e -> list
           )
    in
    let kpairs = List.sort ~cmp:descending kpairs in
    List.map ~f:snd kpairs

  (******************************************************************)

  let get_stats () =
    let today = Stats.round_up_to_day (Unix.gettimeofday ()) in
    let log =
      let maxsize = 90000 in
      let last_month = today -. (31. *. 24. *. 60. *. 60.) in
      Keydb.reverse_logquery ~maxsize last_month
    in
    let size = Keydb.get_num_keys () in
    (log,size)

  let last_stat_page = ref (Stats.generate_html_stats_page_nostats ())

  let calculate_stats_page () =
    plerror 3 "Calculating DB stats";
    let (log,size) = get_stats () in
    last_stat_page := Stats.generate_html_stats_page log size;
    plerror 3 "Done calculating DB stats";
    []

  let get_keys_by_keyid keyid =
    let keyid_length = String.length keyid in
    let short_keyid = String.sub ~pos:(keyid_length - 4) ~len:4 keyid in
    let keys = Keydb.get_by_short_subkeyid short_keyid in
    match keyid_length with
      | 4 -> (* 32-bit keyid.  No further filtering required. *)
          keys

      | 8 -> (* 64-bit keyid *)
          List.filter keys
          ~f:(fun key -> keyid = (Fingerprint.from_key key).Fingerprint.keyid ||
          (** Return keys i& subkeys with matching long keyID *)
             let (mainkeyid,subkeyids) = Fingerprint.keyids_from_key ~short:false key in
             List.exists (fun x -> x = keyid) subkeyids)

      | 20 -> (* 160-bit v. 4 fingerprint *)
          List.filter keys
          ~f:(fun key -> keyid = (Fingerprint.from_key key).Fingerprint.fp ||
          (** Return keys & subkeys with matching fingerprints *)
              let (mainkeyfp,subkeyfps) = Fingerprint.fps_from_key key in
              List.exists (fun x -> x = keyid) subkeyfps)

      | 16 -> (* 128-bit v3 fingerprint.  Not supported *)
          failwith "128-bit v3 fingerprints not implemented"

      | _ -> failwith "unknown keyid type"


  (** returns list of keys readied for presentation *)
  let clean_keys request keys =
    if request.clean
    then Utils.filter_map ~f:Fixkey.presentation_filter keys
    else keys

  (** return uid given keyid *)
  let get_uids request keyid =
    let keys = get_keys_by_keyid keyid in
    let keys = clean_keys request keys in
    match keys with
      | [] | _::_::_ -> []
      | key::tl ->
          let pkey = KeyMerge.key_to_pkey key in
          pkey.KeyMerge.uids

  (******************************************************************)
  (******************************************************************)

  let check_prefix string prefix =
    String.length string >= String.length prefix &&
    (String.sub ~pos:0 ~len:(String.length prefix) string = prefix)

  let lookup_keys search_terms =
    let keys =
      match search_terms with
        | [] -> []
        | first::rest ->
            if check_prefix first "0x" then
              (* keyid search *)
              let keyid_string_length = String.length first - 2 in
              let keyid =
                try
                  KeyHash.dehexify
                    (String.sub ~pos:2 ~len:keyid_string_length first)
                with                e ->
                  let exn_str = sprintf "Unable to parse hash string: %s"
                    (Printexc.to_string e) in
                  raise (Wserver.Misc_error exn_str)
              in
              let keys = (try get_keys_by_keyid keyid
                          with Failure s -> raise (Wserver.Misc_error s))
              in
              keys
            else
              let keys = Keydb.get_by_words ~max:!Settings.max_matches
                           search_terms
              in
              tsort_keys keys
    in
    if keys = [] then raise (Wserver.No_results "No keys found")
    else keys


  (******************************************************************)
  let truncate count keys =
    let rec trunc_c result orig num =
      match orig with
        | [] -> result
        | h::tail ->
            if (num = 0)
            then result
            else (trunc_c (result @ [h]) tail (num-1))
    in
    if count >= 0
    then trunc_c [] keys count
    else keys

  let handle_get_request request =
    match request.kind with
      | Stats ->
          plerror 4 "/pks/lookup: DB Stats request";
          ("text/html; charset=UTF-8", -1, !last_stat_page)
      | Get ->
          plerror 4 "/pks/lookup: Get request (%s)"
            (String.concat " " request.search);
          let keys = lookup_keys request.search in
          let keys = clean_keys request keys in
          let count = List.length keys in
          let keys = truncate request.limit keys in
          let aakeys =
            if keys = [] then ""
            else Armor.encode_pubkey_string (Key.to_string_multiple keys)
          in
          if request.machine_readable then
            ("application/pgp-keys; charset=UTF-8", count, aakeys)
          else
            ("text/html; charset=UTF-8",
             count,
             HtmlTemplates.page
               ~title:(sprintf "Public Key Server -- Get \"%s \""
                         (String.concat ~sep:" " request.search))
               ~body:(sprintf "\r\n<pre>\r\n%s\r\n</pre>\r\n" aakeys)
            )
      | HGet ->
          let hash_str = List.hd request.search in
          plerror 4 "/pks/lookup: Hash search: %s" hash_str;
          let hash = KeyHash.dehexify hash_str in
          flush Pervasives.stdout;
          let key =
            try Keydb.get_by_hash hash with Not_found ->
              raise (Wserver.Misc_error "Requested hash not found")
          in
          let key =
            if request.clean then
              match Fixkey.presentation_filter key with
                  None -> raise (Wserver.Misc_error "No valid key found for hash")
                | Some key -> key
            else key
          in
          let keystr = Key.to_string key in
          let aakey = Armor.encode_pubkey_string keystr
          in
          if request.machine_readable then
            ("application/pgp-keys; charset=UTF-8", 1, aakey)
          else
            ("text/html; charset=UTF-8",
             1,
             HtmlTemplates.page
               ~title:(sprintf "Public Key Server -- Get ``%s ''" hash_str)
               ~body:(sprintf "\r\n<pre>\r\n%s\r\n</pre>\r\n" aakey)
            )

      | Index | VIndex ->
          (* VIndex requests are treated indentically to index requests *)
          plerror 4 "/pks/lookup: Index request: (%s)"
            (String.concat " " request.search);
          let keys = lookup_keys request.search in
          let count = List.length keys in
          let keys = truncate request.limit keys in
          let keys = clean_keys request keys in
          let hashes = List.map ~f:KeyHash.hash keys in
          if request.machine_readable then
            ("text/plain",
             count,
             MRindex.keys_to_index keys)
          else
            begin
              try
                let output =
                  if request.kind = VIndex then
                    List.map2 keys hashes
                      ~f:(Index.key_to_lines_verbose
                            ~get_uids:(get_uids request) request)
                  else
                    List.map2 keys hashes
                      ~f:(Index.key_to_lines_normal request)
                in
                let output = List.flatten output in
                let pre = HtmlTemplates.preformat_list
                            (Index.keyinfo_header request :: output)
                in
                ("text/html; charset=UTF-8",
                 count,
                 HtmlTemplates.page ~body:pre
                   ~title:(sprintf "Search results for '%s'"
                             (String.concat ~sep:" " request.search))
                )

              with
                | Invalid_argument "Insufficiently specific words" ->
                    raise (Wserver.Misc_error
                             ("Insufficiently specific words: provide " ^
                              "at least one more specific keyword"))

                | Invalid_argument "Too many responses" ->
                    raise (Wserver.Misc_error
                             "Too many responses, unable to process query")
            end

  let string_to_oplist s =
    let s = Wserver.strip s in
    try
      let (base,op_string) = chsplit '?' s in
      let oplist = Str.split amp op_string in
      let pairs = List.map ~f:(chsplit '=') oplist in
      let oplist =
        List.map pairs
          ~f:(fun (key,value) -> (key, Wserver.decode value))
      in
      (base,oplist)
    with
        Not_found -> (s,[])

  let get_extension s =
    let pos = String.rindex s '.' in
    s </> (pos,0)

  let bool_to_string b = if b then "true" else "false"
  let print_request cout r =
    fprintf cout "   kind: %s\n" (
      (function
           Index -> "index" | VIndex -> "vindex" | Stats -> "stats"
         | Get -> "get" | HGet -> "hashget")
      r.kind);
    fprintf cout "   fingerprint: %s\n" (bool_to_string r.fingerprint);
    fprintf cout "   exact: %s\n" (bool_to_string r.exact);
    fprintf cout "   search: %s\n"
      (MList.to_string ~f:(fun x -> x) r.search)

  let get_keystrings_from_hashes hashes =
    let rec loop hashes keystrings = match hashes with
        [] -> keystrings
      | hash::tl ->
          try
            let keystring = Keydb.get_keystring_by_hash hash in
            loop tl (keystring::keystrings)
          with
              e ->
                eplerror 2 e "Error fetching key from hash %s"
                (KeyHash.hexify hash);
                loop tl keystrings
    in
    loop hashes []

  let read_file ?(binary=false) fname =
    if not (Sys.file_exists fname) then raise (Wserver.Page_not_found fname);
    let f = (if binary then open_in_bin else open_in) fname in
    protect ~f:(fun () ->
                  let length = in_channel_length f in
                  let buf = String.create length in
                  really_input f buf 0 length;
                  buf
               )
      ~finally:(fun () -> close_in f)


  let is_safe char =
    (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') ||
    (char >= '0' && char <= '9') || (char = '.') || (char = '-')


  let verify_web_fname fname =
    let bad = ref false in
    let pos = ref 0 in
    while not !bad && !pos < String.length fname do
      if not (is_safe fname.[!pos]) then bad := true;
      incr pos
    done;
    not !bad

  let convert_web_fname fname =
    if verify_web_fname fname then
      Filename.concat !Settings.basedir (Filename.concat "web" fname)
    else raise (Wserver.Misc_error "Malformed requst")

  let supported_extensions =
    [ ".jpg",   "image/jpeg";
      ".jpeg",  "image/jpeg";
      ".gif",   "image/gif";
      ".ico",   "image/x-icon";
      ".png",   "image/png";
      ".htm",   "text/html";
      ".html",  "text/html";
      ".txt",   "text/plain";
      ".css",   "text/css";
      ".xhtml", "application/xhtml+xml";
      ".xhtm",  "application/xhtml+xml";
      ".xml",   "application/xhtml+xml";
      ".es",    "application/ecmascript";
      ".js",    "application/javascript";
    ]

  (* Search list for web page index files *)
  let index_files =
    [ "index.html";
      "index.htm";
      "index.xhtml";
      "index.xhtm";
      "index.xml";
    ]

  (** Returns the first element of [index_files] that exists, returning "index.html" if
      none of them exists.  *)
  let index_page_filename =
    let found_files = List.filter index_files
      ~f:(fun x -> Sys.file_exists (convert_web_fname x))
    in
    match found_files with
    | [] -> "index.html"
    | hd :: _ -> hd

  let index_page_mime =
    let period = Str.regexp_string "." in
    let err () =
      raise (Wserver.Misc_error "No mime type found for index page")
    in
    match Str.split period index_page_filename with
    | _ :: ext :: _ ->
      (try List.assoc ("." ^ ext) supported_extensions
       with Not_found -> err ())
    | _ -> err ()

  let webhandler addr msg cout =
    match msg with
      | Wserver.GET (request,headers) ->
          plerror 5 "Get request: %s => %s"
            (sockaddr_to_string addr) request;
          let (base,oplist) = string_to_oplist request in
          if base = "/pks/lookup" then (
            let request = request_of_oplist oplist in
            let (mimetype,count,body) = handle_get_request request in
            cout#write_string body;
            (mimetype, count)
          ) else (
            if (base = "/index.html" || base = "/index.htm"
                || base = "/" || base = "" || base = "/index.xhtml" )
            then
              let fname = convert_web_fname index_page_filename in
              let text = read_file fname in
              cout#write_string text;
              (index_page_mime ^ "; charset=UTF-8", -1)
            else
              (try
                 let extension = get_extension base in
                 let mimetype =
                   try List.assoc extension supported_extensions
                   with Not_found ->
                     raise (Wserver.Misc_error
                              ("internal error: no mimetype " ^
                               "for given extension"))
                 in
                 let base = base </> (1,0) in
                 let data = read_file ~binary:true (convert_web_fname base) in
                 cout#write_string data;
                 (mimetype, -1)
               with
                   Not_found -> raise (Wserver.Page_not_found base)
              )
          )
      | Wserver.POST (request,headers,body) ->
          let request = Wserver.strip request in
          match request with
              "/pks/add" ->
                let keytext = Scanf.sscanf body "keytext=%s" (fun s -> s) in
                let keytext = Wserver.decode keytext in
                let keys = Armor.decode_pubkey keytext in
                plerror 3 "Handling /pks/add for %d keys"
                  (List.length keys);
                cout#write_string "<html><body>";
                let ctr = ref 0 in
                List.iter keys
                  ~f:(fun origkey ->
                        try
                          let key = Fixkey.canonicalize origkey in
                          plerror 3 "/pks/add: key %s added to database"
                            (KeyHash.hexify (KeyHash.hash key));
                          Keydb.add_key_merge ~newkey:true key;
                          incr ctr;
                        with
                          | Fixkey.Bad_key | KeyMerge.Unparseable_packet_sequence ->
                              cout#write_string
                              ("Add failed: Malformed Key --- unexpected packet " ^
                               "type and/or order of packets<br>");
                              plerror 2 "key %s %s"
                                (KeyHash.hexify (KeyHash.hash origkey))
                                "could not be parsed by KeyMerge.canonicalize"
                          | Fixkey.Standalone_revocation_certificate ->
                               cout#write_string ("Add failed: This is a stand-alone " ^
                                                  "revocation certificate. A revocation " ^
                                                  "certificates should be imported to the " ^
                                                  "respective public key before being " ^
                                                  "published to a keyserver");
                          | Bdb.Key_exists as e ->
                              cout#write_string
                              ("Add failed: identical key already " ^
                               "exists in database<br>");
                              eperror e "Key add failed"
                          | e ->
                              Eventloop.reraise e;
                              cout#write_string "Add failed<br>";
                              eperror e "Key add failed"
                     );
                if !ctr > 0 then (
                  cout#write_string
                    ("Key block added to key server database.\n  " ^
                     "New public keys added: <br>");
                  cout#write_string (sprintf "%d key(s) added successfully.<br>" !ctr)
                );
                cout#write_string "</html></body>";
                ("text/html; charset=UTF-8", List.length keys)
            | "/pks/hashquery" ->
                plerror 4 "Handling /pks/hashquery";
                let sin = new Channel.string_in_channel body 0 in
                let hashes =
                  CMarshal.unmarshal_list ~f:CMarshal.unmarshal_string sin
                in
                let keystrings = get_keystrings_from_hashes hashes in
                perror "%d keys found" (List.length keystrings);
                CMarshal.marshal_list ~f:CMarshal.marshal_string cout
                  keystrings;
                ("pgp/keys" (* This is a bogus content-type *),
                 List.length keystrings)
            | _ ->
                cout#write_string (HtmlTemplates.page
                                     ~title:"Unexpected POST request"
                                     ~body:"");
                ("text/html; charset=UTF-8", -1)


  (** Prepare handler for use with eventloop by transforming system
    channels to Channel objects and by returning empty list instead
    of unit *)
  let eventify_handler handle =
    (fun addr cin cout ->
       let cin = (new Channel.sys_in_channel cin)
       and cout = (new Channel.sys_out_channel cout) in
       handle addr cin cout;
       []
    )

  let get_filters =
    Utils.unit_memoize
      (fun () ->
         try Str.split comma_rxp (Keydb.get_meta "filters")
         with Not_found -> []
      )


  (** Handler for commands coming off of the db_command_addr *)
  let command_handler addr cin cout =
    match (unmarshal cin).msg with
      | LogQuery (count,timestamp) ->
          let logresp = Keydb.logquery ~maxsize:count timestamp in
          let length = List.length logresp in
          if length > 0 then
            plerror 3 "Sending LogResp size %d" length;
          marshal cout (LogResp logresp)

      | WordQuery words ->
          plerror 3 "Handling WordQuery";
          let keys = Keydb.get_by_words ~max:!Settings.max_matches words in
          marshal cout (Keys keys)

      | Keys keys ->
          let keys = List.fold_left ~init:[] keys
                       ~f:(fun list key ->
                             try (Fixkey.canonicalize key)::list
                             with KeyMerge.Unparseable_packet_sequence | Fixkey.Bad_key -> list
                          )
          in
          marshal cout (Ack 0);
          (try Keydb.add_keys_merge keys
           with e -> eplerror 2 e "Key addition failed")

      | DeleteKey hash ->
          plerror 3 "Handling DeleteKey";
          ( try
              let hash = RMisc.truncate hash KeyHash.hash_bytes in
              let key = Keydb.get_by_hash hash in
              Keydb.delete_key ~hash key;
              marshal cout (Ack 0);
            with
                e ->
                  marshal cout (Ack (-1));
                  raise e
          )
      | HashRequest hashes ->
          plerror 3 "Handling HashRequest";
          let keys =
            List.fold_left hashes ~init:[]
              ~f:(fun list hash ->
                    try (Keydb.get_by_hash hash)::list
                    with
                        Not_found ->
                          plerror 2 "Requested key %s not found"
                          (Utils.hexstring hash);
                          list
                 )
          in
          plerror 3 "Returning set of %d keys" (List.length keys);
          marshal cout (Keys keys)


      | Config (s,cvar) ->
          plerror 4 "Received config message";
          (match (s,cvar) with
             | ("checkpoint", `none) ->
                 checkpoint ()
             | ("filters", `none) ->
                 marshal cout (Filters (get_filters ()))
             | (str,value) ->
                 perror "Unexpected config request <%s>" str
          )


      | m ->
          marshal cout ProtocolError;
          perror "Unexpected (%s) message" (msg_to_string m)


  (***********************************************************************)

  (** dequeues and transmits single key.  Returns true if there
    might be more keys to be handled. *)
  let rec transmit_single_key () =
    let txn = Keydb.txn_begin () in
    try
      match (try Some (Keydb.dequeue_key ~txn)
             with Not_found -> None)
      with
        | Some (time,key) ->
            let body = Armor.encode_pubkey key in
            let to_header = ("To", String.concat ~sep:", "
                               (Membership.get_mailsync_partners ()))
            in
            let msg = { Sendmail.headers =
                          [ to_header;
                            "From", Settings.get_from_addr ();
                            "Reply-To", Settings.get_from_addr ();
                            "Errors-To", Settings.get_from_addr ();
                            "Subject","incremental";
                            "Precedence","list";
                            "Content-type", "application/pgp-keys";
                            "X-KeyServer-Sent", Settings.get_from_addr ();
                          ] ;
                        Sendmail.body = body;
                      }
            in
            let string = Sendmail.msg_to_string msg in
            plerror 3 "Message transmitted for key %s"
              (KeyHash.hexify (KeyHash.hash key));
            plerror 6 "%s" string;
            Sendmail.send msg;
            Keydb.txn_commit txn;
            plerror 5 "transmission queue transaction committed";
            true
        | None ->
            (* nothing was done, so commiting and aborting are same here *)
            Keydb.txn_abort txn;
            false
      with
          e ->
            Keydb.txn_abort txn;
            raise e


  (** Transmit all enqueued keys to other hosts *)
  let transmit_keys () =
    while transmit_single_key () do () done;
    []

  (***********************************************************************)

  let sync_db_on_sig () =
    sync ();
    checkpoint ()

  let () = Sys.set_signal Sys.sigusr1
          (Sys.Signal_handle (fun _ -> sync_db_on_sig ()))

  let () = Sys.set_signal Sys.sigusr2
      (Sys.Signal_handle (fun _ ->
        Eventloop.add_events Eventloop.heap
          [Eventloop.Event(0.0, Eventloop.Callback calculate_stats_page)]))

  (***********************************************************************)

  let run () =
    Keydb.open_dbs settings;
    if !Settings.initial_stat then ignore (calculate_stats_page ());
    plerror 2 "Database opened";
    plerror 0 "Applied filters: %s" (String.concat ~sep:", "
                                       (get_filters ()));
    Eventloop.evloop

      (
        (if withtxn
         then (Ehandlers.repeat_forever_simple checkpoint_interval checkpoint)
         else (Ehandlers.repeat_forever_simple sync_interval sync))
        @
          Ehandlers.repeat_forever_simple !Settings.membership_reload_time
          Membership.reset_membership_time
        @
          (if !Settings.send_mailsyncs then
             (Ehandlers.repeat_forever 10.
                (Eventloop.make_tc ~cb:transmit_keys ~timeout:0
                   ~name:"mail transmit keys" )
             )
           else [])
        @
          (Ehandlers.repeat_forever 10.
             (Eventloop.make_tc ~name:"mailsync" ~timeout:0
                ~cb:(Mailsync.load_mailed_keys
                       ~addkey:(Keydb.add_key_merge ~newkey:false)))
          )
        @
          (Ehandlers.repeat_at_hour !Settings.stat_calc_hour
             calculate_stats_page)
      )

      (
         (comsock, Eventloop.make_th ~name:"command handler"
            ~timeout:!Settings.command_timeout
            ~cb:(eventify_handler command_handler))
        ::
         (List.map websocks
            ~f:(fun sock ->
                  (sock, Eventloop.make_th ~name:"webserver"
                     ~timeout:!Settings.wserver_timeout
                     ~cb:(Wserver.accept_connection webhandler ~recover_timeout:1))))
      )



  let run () =
    protect ~f:run
      ~finally:(fun () ->
                  set_catch_break false;
                  plerror 0 "Shutting down database";
                  Keydb.sync ();
                  plerror 0 "Database sync'd";
                  Keydb.unconditional_checkpoint ();
                  plerror 0 "Database checkpointed";
                  Keydb.close_dbs ();
                  plerror 0 "Database closed"
               )
end
