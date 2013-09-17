(***********************************************************************)
(* reconserver.ml - Executable: server process that handles            *)
(*                  reconciliation                                     *)
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
  open DbMessages
  module Unix = UnixLabels
  module PTree = PrefixTree
  module Map = PMap.Map
  module ZSet = ZZp.Set

  open RecoverList
  open PTreeDB
  open Catchup

  let settings = {
    mbar = !Settings.mbar;
    bitquantum = !Settings.bitquantum;
    treetype = (if !Settings.transactions
                then `transactional
                else if !Settings.disk_ptree
                then `ondisk else `inmem);
    max_nodes = !Settings.max_ptree_nodes;
    dbdir = Lazy.force Settings.ptree_dbdir;
    cache_bytes = !Settings.ptree_cache_bytes;
    pagesize = !Settings.ptree_pagesize;
  }

  (******************************************************************)

  let reconsocks =
    List.rev_map ~f:Eventloop.maybe_create_sock (make_addr_list recon_address recon_port)
  let reconsocks =
    List.fold_right ~init:[]
      ~f:(function
	   | Some sock -> fun acc -> sock :: acc
	   | None -> fun acc -> acc)
      reconsocks
  let () =
    if reconsocks = [] then
      failwith "Could not listen on any address."



  let () =
    if Sys.file_exists recon_command_name
    then Unix.unlink recon_command_name
  let comsock = Eventloop.create_sock recon_command_addr

  let filters = ref None
  let get_filters () = match !filters with
      None -> failwith "No filters retrieved"
    | Some filters -> filters


  (***************************************************************)
  (*  Handlers  *************************************************)
  (***************************************************************)

  let eventify_handler handle =
    (fun addr cin cout ->
       let cin = (new Channel.sys_in_channel cin)
       and cout = (new Channel.sys_out_channel cout) in
       handle addr cin cout
    )

  let choose_partner () =
    try
      let addrlist = Membership.choose () in
      (* Only return usable addresses *)
      let is_compatible addr =
        try
          ignore (match_client_recon_addr addr.Unix.ai_addr);
          true
        with Not_found -> false
      in
      let addrlist = List.filter ~f:is_compatible addrlist in
      List.nth addrlist (Random.int (List.length addrlist))
    with
        Not_found | Invalid_argument _ ->
          failwith "No gossip partners available"

  let missing_keys_timeout = !Settings.missing_keys_timeout

  (******************************************************************)

  let rec get_missing_keys () =
    let name = "get missing keys" in
    let timeout = missing_keys_timeout in
    try

      ( try
          let (hashes,httpaddr) = Queue.pop recover_list in
          plerror 3
            "Requesting %d missing keys from %s, starting with %s"
            (List.length hashes) (sockaddr_to_string httpaddr)
            (match hashes with
                 [] -> "<nohash>"
               | hash::tl -> KeyHash.hexify hash
            );

          let keystrings = ReconComm.get_keystrings_via_http httpaddr hashes in
          plerror 3 "%d keys received" (List.length keystrings);
          let ack = ReconComm.send_dbmsg (KeyStrings keystrings) in
          if ack <> Ack 0
          then failwith ("Reconserver.get_missing_keys: " ^
                         "Unexpected reply to KeyStrings message");
          let now = Unix.gettimeofday () in
          [
            Eventloop.Event
             (now,
              Eventloop.make_tc
                ~name:"get_missing_keys.catchup"
                ~timeout:max_int
                ~cb:Catchup.catchup);

            Eventloop.Event
              (Ehandlers.float_incr now,
               Eventloop.make_tc ~name ~timeout
                 ~cb:get_missing_keys; );
          ]
        with
          | Queue.Empty -> enable_gossip (); []
          | Eventloop.SigAlarm as e -> raise e
          | e ->
              Eventloop.reraise e;
              eperror e "Error getting missing keys";
              [Eventloop.Event (Unix.gettimeofday (),
                                Eventloop.make_tc ~cb:get_missing_keys
                                  ~timeout ~name)
              ]

      )
    with
      | Eventloop.SigAlarm ->
          plerror 2 "get_missing_keys terminated by timeout";
          (* If we time out, just schedule the next one *)
          [Eventloop.Event (Unix.gettimeofday (),
                            Eventloop.make_tc ~cb:get_missing_keys ~timeout ~name; ) ]

  (******************************************************************)

  (** convert a sockaddr to a string suitable for including in a file name *)
  let sockaddr_to_name sockaddr = match sockaddr with
      Unix.ADDR_UNIX s -> sprintf "UNIX_%s" s
    | Unix.ADDR_INET (addr,p) -> sprintf "%s_%d" (Unix.string_of_inet_addr addr) p

  (******************************************************************)

  (** Handles incoming reconciliation *)
  let recon_handler addr cin cout =
    if gossip_disabled ()  then
      begin
        plerror 3
          "Reconciliation attempt from %s while gossip disabled. %s"
          (sockaddr_to_string addr) "Ignoring.";
        []
      end
    else if not (Membership.test addr) then
      begin
        plerror 1
          "Reconciliation attempt from unauthorized host %s.  Ignoring"
          (sockaddr_to_string addr) ;
        []
      end
    else
      begin
        plerror 4 "Beginning recon as server, client: %s"
          (sockaddr_to_string addr);
        let cin = (new Channel.sys_in_channel cin)
        and cout = (new Channel.sys_out_channel cout) in
        let filters = get_filters () in
        let (results,http_addr) =
          ReconCS.handle_connection (get_ptree ()) ~filters
            ~partner:addr cin cout
        in
        plerror 4 "Reconciliation complete";
        let elements = ZSet.elements results in
        let hashes = hashconvert elements in
        print_hashes (sockaddr_to_string http_addr) hashes;
        log_diffs (sprintf "diff-%s.txt" (sockaddr_to_name http_addr)) hashes;
        if List.length elements > 0
        then
          begin
            update_recover_list elements http_addr;
            [Eventloop.Event (Unix.gettimeofday () +. 10.0,
                              Eventloop.make_tc ~cb:get_missing_keys
                                ~timeout:missing_keys_timeout
                                ~name:"get missing keys"
                             )]
          end
        else
          []
      end


  (******************************************************************)

  (** Initiates reconciliation as client *)
  let initiate_recon () =
    if gossip_disabled () then
      begin
        plerror 5 "Not gossiping because gossip is disabled";
        []
      end
    else
      begin
        let partner = choose_partner () in
        plerror 4 "Recon partner: %s" (sockaddr_to_string partner.Unix.ai_addr);
        let filters = get_filters () in
        let (results,http_addr) =
          ReconCS.connect (get_ptree ()) ~filters ~partner
        in
        let results = ZSet.elements results in
        plerror 4 "Reconciliation complete";
        let hashes = hashconvert results in
        print_hashes (sockaddr_to_string http_addr) hashes;
        log_diffs (sprintf "diff-%s.txt" (sockaddr_to_name http_addr)) hashes;
        match results with
            [] -> []
          | _ ->
              update_recover_list results http_addr;
              [Eventloop.Event (Unix.gettimeofday (),
                                Eventloop.make_tc ~cb:get_missing_keys
                                  ~timeout:missing_keys_timeout
                                  ~name:"get missing keys"
                               )]
      end


  (******************************************************************)

  let command_handler addr cin cout =
    match (unmarshal cin).msg with

      | Synchronize ->
          marshal cout (Ack 0);
          plerror 2 "Initiating recon due to explicit request";
          initiate_recon ()

      | RandomDrop n ->
          marshal cout (Ack 0);
          for i = 1 to n do
            try
              let hash = PTree.get_random (get_ptree ())
                           (PTree.root (get_ptree ())) in
              let hash = RMisc.truncate hash KeyHash.hash_bytes in
              plerror 3 "Requesting deletion %s" (Utils.hexstring hash);
              ignore (ReconComm.send_dbmsg (DeleteKey hash))
            with
                Not_found ->
                  failwith "Attempted to delete element from empty prefix tree"
              | e ->
                  Eventloop.reraise e;
                  eplerror 3 e "Attempt to delete key failed"
          done;
          []

      | HashRequest hashes ->
          let keyresp = (ReconComm.send_dbmsg (HashRequest hashes)) in
          assert (match keyresp with Keys _ -> true | _ -> false);
          marshal cout keyresp;
          []

      | Config (s,cvar) ->
          plerror 4 "Received config message";
          (match (s,cvar) with
               ("maxnodes",`int x) ->
                 plerror 3 "Setting maxnodes to %d" x;
                 let txn = new_txnopt () in
                 (try
                    PTree.set_maxnodes (get_ptree ()) txn x;
                    PTree.clean txn (get_ptree ());
                    commit_txnopt txn
                  with
                      e ->
                        eplerror 1 e "set_maxnodes Transaction aborting";
                        abort_txnopt txn)
             | _ ->
                 failwith "Unexpected config request"
          );
          []

      | m ->
          marshal cout ProtocolError;
          perror "Unexpected message: %s" (msg_to_string m);
          []

  (***************************************************************)

  let sync_interval = !Settings.recon_sync_interval
  let sync_tree () =
    perror "Syncing prefix tree";
    let txn = new_txnopt () in
    try
      PTree.clean txn (get_ptree ());
      commit_txnopt txn
    with
        e ->
          eplerror 1 e "sync_tree transaction aborting";
          abort_txnopt txn;
          raise e


  let checkpoint_interval = !Settings.recon_checkpoint_interval

  (***************************************************************)

  let () = Sys.set_signal Sys.sigusr1 Sys.Signal_ignore
  let () = Sys.set_signal Sys.sigusr2 Sys.Signal_ignore

  (***********************************************************************)

  let prepare () =
    set_logfile "recon";
    plerror 1 "sks_recon, SKS version %s%s"  version version_suffix;
    plerror 0 "Using BerkelyDB version %s" (Bdb.version(););
    plerror 1 "Copyright Yaron Minsky 2002-2013";
    plerror 1 "Licensed under GPL.  See LICENSE file for details";
    plerror 5 "recon port: %d" recon_port;

    init_db settings;
    init_ptree settings


  let run () =
    prepare ();
    plerror 4 "Initiating catchup";
    uninterruptable_catchup ();
    (* do initial catchup to ensure reconciliation data
       is synchronized with key database *)
    plerror 4 "Fetching filters";
    filters := Some (ReconComm.fetch_filters ());
    plerror 4 "Starting event loop";
    Eventloop.evloop
      ( [ Eventloop.Event (0.0, Eventloop.Callback catchup) ]
        @ (Ehandlers.repeat_forever_simple catchup_interval catchup)
        @ (if !Settings.gossip
           then Ehandlers.repeat_forever
             ~jitter:0.1 (* 10% randomness in delay interval *)
             !Settings.gossip_interval
             (Eventloop.make_tc
                ~cb:initiate_recon
                ~name:"recon as client"
                ~timeout:!Settings.reconciliation_config_timeout
             )
           else [] )
        @ (match settings.treetype with
             | `transactional ->
                 Ehandlers.repeat_forever_simple checkpoint_interval checkpoint
             | `ondisk -> Ehandlers.repeat_forever_simple
                 sync_interval sync_tree
             | `inmem -> []
          )
      )

      ( (comsock, Eventloop.make_th
           ~name:"command handler"
           ~cb:(eventify_handler command_handler)
           ~timeout:!Settings.command_timeout
        )
       ::
        (List.map ~f:(fun sock ->
          (sock, Eventloop.make_th
             ~name:"reconciliation handler"
             ~cb:recon_handler
             ~timeout:!Settings.reconciliation_config_timeout))
           reconsocks))


  (******************************************************************)

  let run () =
    protect ~f:run
      ~finally:(fun () ->
                  closedb ();
                  plerror 2 "DB closed"
               )

end
