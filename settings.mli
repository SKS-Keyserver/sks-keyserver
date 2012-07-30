val n : int ref
val set_n : int -> unit
val debug : bool ref
val set_debug : bool -> unit
val debuglevel : int ref
val set_debuglevel : int -> unit
val mbar : int ref
val set_mbar : int -> unit
val bitquantum : int ref
val set_bitquantum : int -> unit
val drop : int ref
val set_drop : int -> unit
val bytes : int ref
val set_bytes : int -> unit
val max_recover : int ref
val set_max_recover : int -> unit
val seed : int ref
val self_seed : bool ref
val set_seed : int -> unit
val recon_port : int ref
val recon_address : string ref
val set_recon_address : string -> unit
val hkp_port : int ref
val hkp_address : string ref
val set_hkp_address : string -> unit
val use_port_80 : bool ref
val set_base_port : int -> unit
val set_recon_port : int -> unit
val set_hkp_port : int -> unit
val setup_RNG : unit -> unit
val max_internal_matches : int ref
val set_max_internal_matches : int -> unit
val max_matches : int ref
val set_max_matches : int -> unit
val max_outstanding_recon_requests : int ref
val set_max_outstanding_recon_requests : int -> unit
val max_uid_fetches : int ref
val set_max_uid_fetches : int -> unit
val dump_new : bool ref
val disk_ptree : bool ref
val max_ptree_nodes : int ref
val set_max_ptree_nodes : int -> unit
val http_fetch_size : int ref
val set_http_fetch_size : int -> unit
val prob : float ref
val set_prob : float -> unit
val db_sync_interval : float ref
val set_db_sync_interval : float -> unit
val recon_sync_interval : float ref
val set_recon_sync_interval : float -> unit
val gossip_interval : float ref
val set_gossip_interval : float -> unit
val gossip : bool ref
val anonlist : string list ref
val cache_bytes : int option ref
val set_cache_bytes : int -> unit
val pagesize : int option ref
val set_pagesize : int -> unit
val keyid_pagesize : int option ref
val set_keyid_pagesize : int -> unit
val meta_pagesize : int option ref
val set_meta_pagesize : int -> unit
val subkeyid_pagesize : int option ref
val set_subkeyid_pagesize : int -> unit
val time_pagesize : int option ref
val set_time_pagesize : int -> unit
val tqueue_pagesize : int option ref
val set_tqueue_pagesize : int -> unit
val word_pagesize : int option ref
val set_word_pagesize : int -> unit
val ptree_cache_bytes : int option ref
val set_ptree_cache_bytes : int -> unit
val ptree_pagesize : int option ref
val set_ptree_pagesize : int -> unit
val hostname : string ref
val nodename : string ref
val server_contact : string ref
val set_hostname : string -> unit
val filelog : bool ref
val transactions : bool ref
val checkpoint_interval : float ref
val set_checkpoint_interval : float -> unit
val recon_checkpoint_interval : float ref
val set_recon_checkpoint_interval : float -> unit
val ptree_thresh_mult : int ref
val set_ptree_thresh_mult : int -> unit
val recon_thresh_mult : int ref
val set_recon_thresh_mult : int -> unit
val wserver_timeout : int ref
val set_wserver_timeout : int -> unit
val reconciliation_config_timeout : int ref
val set_reconciliation_config_timeout : int -> unit
val reconciliation_timeout : int ref
val set_reconciliation_timeout : int -> unit
val initial_stat : bool ref
val stat_calc_hour : int ref
val set_stat_calc_hour : int -> unit
val missing_keys_timeout : int ref
val set_missing_keys_timeout : int -> unit
val command_timeout : int ref
val set_command_timeout : int -> unit
val sendmail_cmd : string ref
val set_sendmail_cmd : string -> unit
val membership_reload_time : float ref
val set_membership_reload_time : float -> unit
val send_mailsyncs : bool ref
val log_diffs : bool ref
val from_addr : string option ref
val set_from_addr : string -> unit
val get_from_addr : unit -> string
val use_stdin : bool ref
val basedir : string ref
val base_dbdir : string
val base_ptree_dbdir : string
val base_membership_file : string
val base_mailsync_file : string
val base_dumpdir : string
val base_msgdir : string
val base_failed_msgdir : string
val dbdir : string lazy_t
val ptree_dbdir : string lazy_t
val membership_file : string lazy_t
val mailsync_file : string lazy_t
val dumpdir : string lazy_t
val msgdir : string lazy_t
val failed_msgdir : string lazy_t
val parse_spec : (Arg.key * Arg.spec * Arg.doc) list
val anon_options : string -> unit
val usage_string : string
