module Unix :
  sig
    type error =
      Unix.error =
        E2BIG
      | EACCES
      | EAGAIN
      | EBADF
      | EBUSY
      | ECHILD
      | EDEADLK
      | EDOM
      | EEXIST
      | EFAULT
      | EFBIG
      | EINTR
      | EINVAL
      | EIO
      | EISDIR
      | EMFILE
      | EMLINK
      | ENAMETOOLONG
      | ENFILE
      | ENODEV
      | ENOENT
      | ENOEXEC
      | ENOLCK
      | ENOMEM
      | ENOSPC
      | ENOSYS
      | ENOTDIR
      | ENOTEMPTY
      | ENOTTY
      | ENXIO
      | EPERM
      | EPIPE
      | ERANGE
      | EROFS
      | ESPIPE
      | ESRCH
      | EXDEV
      | EWOULDBLOCK
      | EINPROGRESS
      | EALREADY
      | ENOTSOCK
      | EDESTADDRREQ
      | EMSGSIZE
      | EPROTOTYPE
      | ENOPROTOOPT
      | EPROTONOSUPPORT
      | ESOCKTNOSUPPORT
      | EOPNOTSUPP
      | EPFNOSUPPORT
      | EAFNOSUPPORT
      | EADDRINUSE
      | EADDRNOTAVAIL
      | ENETDOWN
      | ENETUNREACH
      | ENETRESET
      | ECONNABORTED
      | ECONNRESET
      | ENOBUFS
      | EISCONN
      | ENOTCONN
      | ESHUTDOWN
      | ETOOMANYREFS
      | ETIMEDOUT
      | ECONNREFUSED
      | EHOSTDOWN
      | EHOSTUNREACH
      | ELOOP
      | EOVERFLOW
      | EUNKNOWNERR of int
    exception Unix_error of error * string * string
    val error_message : error -> string
    val handle_unix_error : ('a -> 'b) -> 'a -> 'b
    val environment : unit -> string array
    val getenv : string -> string
    val putenv : string -> string -> unit
    type process_status =
      Unix.process_status =
        WEXITED of int
      | WSIGNALED of int
      | WSTOPPED of int
    type wait_flag = Unix.wait_flag = WNOHANG | WUNTRACED
    val execv : prog:string -> args:string array -> 'a
    val execve : prog:string -> args:string array -> env:string array -> 'a
    val execvp : prog:string -> args:string array -> 'a
    val execvpe : prog:string -> args:string array -> env:string array -> 'a
    val fork : unit -> int
    val wait : unit -> int * process_status
    val waitpid : mode:wait_flag list -> int -> int * process_status
    val system : string -> process_status
    val getpid : unit -> int
    val getppid : unit -> int
    val nice : int -> int
    type file_descr = Unix.file_descr
    val stdin : file_descr
    val stdout : file_descr
    val stderr : file_descr
    type open_flag =
      Unix.open_flag =
        O_RDONLY
      | O_WRONLY
      | O_RDWR
      | O_NONBLOCK
      | O_APPEND
      | O_CREAT
      | O_TRUNC
      | O_EXCL
      | O_NOCTTY
      | O_DSYNC
      | O_SYNC
      | O_RSYNC
    type file_perm = int
    val openfile :
      string -> mode:open_flag list -> perm:file_perm -> file_descr
    val close : file_descr -> unit
    val read : file_descr -> buf:string -> pos:int -> len:int -> int
    val write : file_descr -> buf:string -> pos:int -> len:int -> int
    val single_write : file_descr -> buf:string -> pos:int -> len:int -> int
    val in_channel_of_descr : file_descr -> in_channel
    val out_channel_of_descr : file_descr -> out_channel
    val descr_of_in_channel : in_channel -> file_descr
    val descr_of_out_channel : out_channel -> file_descr
    type seek_command = Unix.seek_command = SEEK_SET | SEEK_CUR | SEEK_END
    val lseek : file_descr -> int -> mode:seek_command -> int
    val truncate : string -> len:int -> unit
    val ftruncate : file_descr -> len:int -> unit
    type file_kind =
      Unix.file_kind =
        S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
    type stats =
      Unix.stats = {
      st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int;
      st_atime : float;
      st_mtime : float;
      st_ctime : float;
    }
    val stat : string -> stats
    val lstat : string -> stats
    val fstat : file_descr -> stats
    val isatty : file_descr -> bool
    module LargeFile :
      sig
        val lseek : file_descr -> int64 -> mode:seek_command -> int64
        val truncate : string -> len:int64 -> unit
        val ftruncate : file_descr -> len:int64 -> unit
        type stats =
          Unix.LargeFile.stats = {
          st_dev : int;
          st_ino : int;
          st_kind : file_kind;
          st_perm : file_perm;
          st_nlink : int;
          st_uid : int;
          st_gid : int;
          st_rdev : int;
          st_size : int64;
          st_atime : float;
          st_mtime : float;
          st_ctime : float;
        }
        val stat : string -> stats
        val lstat : string -> stats
        val fstat : file_descr -> stats
      end
    val unlink : string -> unit
    val rename : src:string -> dst:string -> unit
    val link : src:string -> dst:string -> unit
    type access_permission =
      Unix.access_permission =
        R_OK
      | W_OK
      | X_OK
      | F_OK
    val chmod : string -> perm:file_perm -> unit
    val fchmod : file_descr -> perm:file_perm -> unit
    val chown : string -> uid:int -> gid:int -> unit
    val fchown : file_descr -> uid:int -> gid:int -> unit
    val umask : int -> int
    val access : string -> perm:access_permission list -> unit
    val dup : file_descr -> file_descr
    val dup2 : src:file_descr -> dst:file_descr -> unit
    val set_nonblock : file_descr -> unit
    val clear_nonblock : file_descr -> unit
    val set_close_on_exec : file_descr -> unit
    val clear_close_on_exec : file_descr -> unit
    val mkdir : string -> perm:file_perm -> unit
    val rmdir : string -> unit
    val chdir : string -> unit
    val getcwd : unit -> string
    val chroot : string -> unit
    type dir_handle = Unix.dir_handle
    val opendir : string -> dir_handle
    val readdir : dir_handle -> string
    val rewinddir : dir_handle -> unit
    val closedir : dir_handle -> unit
    val pipe : unit -> file_descr * file_descr
    val mkfifo : string -> perm:file_perm -> unit
    val create_process :
      prog:string ->
      args:string array ->
      stdin:file_descr -> stdout:file_descr -> stderr:file_descr -> int
    val create_process_env :
      prog:string ->
      args:string array ->
      env:string array ->
      stdin:file_descr -> stdout:file_descr -> stderr:file_descr -> int
    val open_process_in : string -> in_channel
    val open_process_out : string -> out_channel
    val open_process : string -> in_channel * out_channel
    val open_process_full :
      string -> env:string array -> in_channel * out_channel * in_channel
    val close_process_in : in_channel -> process_status
    val close_process_out : out_channel -> process_status
    val close_process : in_channel * out_channel -> process_status
    val close_process_full :
      in_channel * out_channel * in_channel -> process_status
    val symlink : src:string -> dst:string -> unit
    val readlink : string -> string
    val select :
      read:file_descr list ->
      write:file_descr list ->
      except:file_descr list ->
      timeout:float -> file_descr list * file_descr list * file_descr list
    type lock_command =
      Unix.lock_command =
        F_ULOCK
      | F_LOCK
      | F_TLOCK
      | F_TEST
      | F_RLOCK
      | F_TRLOCK
    val lockf : file_descr -> mode:lock_command -> len:int -> unit
    val kill : pid:int -> signal:int -> unit
    type sigprocmask_command =
      Unix.sigprocmask_command =
        SIG_SETMASK
      | SIG_BLOCK
      | SIG_UNBLOCK
    val sigprocmask : mode:sigprocmask_command -> int list -> int list
    val sigpending : unit -> int list
    val sigsuspend : int list -> unit
    val pause : unit -> unit
    type process_times =
      Unix.process_times = {
      tms_utime : float;
      tms_stime : float;
      tms_cutime : float;
      tms_cstime : float;
    }
    type tm =
      Unix.tm = {
      tm_sec : int;
      tm_min : int;
      tm_hour : int;
      tm_mday : int;
      tm_mon : int;
      tm_year : int;
      tm_wday : int;
      tm_yday : int;
      tm_isdst : bool;
    }
    val time : unit -> float
    val gettimeofday : unit -> float
    val gmtime : float -> tm
    val localtime : float -> tm
    val mktime : tm -> float * tm
    val alarm : int -> int
    val sleep : int -> unit
    val times : unit -> process_times
    val utimes : string -> access:float -> modif:float -> unit
    type interval_timer =
      Unix.interval_timer =
        ITIMER_REAL
      | ITIMER_VIRTUAL
      | ITIMER_PROF
    type interval_timer_status =
      Unix.interval_timer_status = {
      it_interval : float;
      it_value : float;
    }
    val getitimer : interval_timer -> interval_timer_status
    val setitimer :
      interval_timer -> interval_timer_status -> interval_timer_status
    val getuid : unit -> int
    val geteuid : unit -> int
    val setuid : int -> unit
    val getgid : unit -> int
    val getegid : unit -> int
    val setgid : int -> unit
    val getgroups : unit -> int array
    val setgroups : int array -> unit
    val initgroups : string -> int -> unit
    type passwd_entry =
      Unix.passwd_entry = {
      pw_name : string;
      pw_passwd : string;
      pw_uid : int;
      pw_gid : int;
      pw_gecos : string;
      pw_dir : string;
      pw_shell : string;
    }
    type group_entry =
      Unix.group_entry = {
      gr_name : string;
      gr_passwd : string;
      gr_gid : int;
      gr_mem : string array;
    }
    val getlogin : unit -> string
    val getpwnam : string -> passwd_entry
    val getgrnam : string -> group_entry
    val getpwuid : int -> passwd_entry
    val getgrgid : int -> group_entry
    type inet_addr = Unix.inet_addr
    val inet_addr_of_string : string -> inet_addr
    val string_of_inet_addr : inet_addr -> string
    val inet_addr_any : inet_addr
    val inet_addr_loopback : inet_addr
    val inet6_addr_any : inet_addr
    val inet6_addr_loopback : inet_addr
    type socket_domain = Unix.socket_domain = PF_UNIX | PF_INET | PF_INET6
    type socket_type =
      Unix.socket_type =
        SOCK_STREAM
      | SOCK_DGRAM
      | SOCK_RAW
      | SOCK_SEQPACKET
    type sockaddr =
      Unix.sockaddr =
        ADDR_UNIX of string
      | ADDR_INET of inet_addr * int
    val socket :
      domain:socket_domain -> kind:socket_type -> protocol:int -> file_descr
    val domain_of_sockaddr : sockaddr -> socket_domain
    val socketpair :
      domain:socket_domain ->
      kind:socket_type -> protocol:int -> file_descr * file_descr
    val accept : file_descr -> file_descr * sockaddr
    val bind : file_descr -> addr:sockaddr -> unit
    val connect : file_descr -> addr:sockaddr -> unit
    val listen : file_descr -> max:int -> unit
    type shutdown_command =
      Unix.shutdown_command =
        SHUTDOWN_RECEIVE
      | SHUTDOWN_SEND
      | SHUTDOWN_ALL
    val shutdown : file_descr -> mode:shutdown_command -> unit
    val getsockname : file_descr -> sockaddr
    val getpeername : file_descr -> sockaddr
    type msg_flag = Unix.msg_flag = MSG_OOB | MSG_DONTROUTE | MSG_PEEK
    val recv :
      file_descr ->
      buf:string -> pos:int -> len:int -> mode:msg_flag list -> int
    val recvfrom :
      file_descr ->
      buf:string ->
      pos:int -> len:int -> mode:msg_flag list -> int * sockaddr
    val send :
      file_descr ->
      buf:string -> pos:int -> len:int -> mode:msg_flag list -> int
    val sendto :
      file_descr ->
      buf:string ->
      pos:int -> len:int -> mode:msg_flag list -> addr:sockaddr -> int
    type socket_bool_option =
      UnixLabels.socket_bool_option =
        SO_DEBUG
      | SO_BROADCAST
      | SO_REUSEADDR
      | SO_KEEPALIVE
      | SO_DONTROUTE
      | SO_OOBINLINE
      | SO_ACCEPTCONN
      | TCP_NODELAY
      | IPV6_ONLY
    type socket_int_option =
      UnixLabels.socket_int_option =
        SO_SNDBUF
      | SO_RCVBUF
      | SO_ERROR
      | SO_TYPE
      | SO_RCVLOWAT
      | SO_SNDLOWAT
    type socket_optint_option = UnixLabels.socket_optint_option = SO_LINGER
    type socket_float_option =
      UnixLabels.socket_float_option =
        SO_RCVTIMEO
      | SO_SNDTIMEO
    val getsockopt : file_descr -> socket_bool_option -> bool
    val setsockopt : file_descr -> socket_bool_option -> bool -> unit
    val getsockopt_int : file_descr -> socket_int_option -> int
    val setsockopt_int : file_descr -> socket_int_option -> int -> unit
    val getsockopt_optint : file_descr -> socket_optint_option -> int option
    val setsockopt_optint :
      file_descr -> socket_optint_option -> int option -> unit
    val getsockopt_float : file_descr -> socket_float_option -> float
    val setsockopt_float : file_descr -> socket_float_option -> float -> unit
    val getsockopt_error : file_descr -> error option
    val open_connection : sockaddr -> in_channel * out_channel
    val shutdown_connection : in_channel -> unit
    val establish_server :
      (in_channel -> out_channel -> unit) -> addr:sockaddr -> unit
    type host_entry =
      Unix.host_entry = {
      h_name : string;
      h_aliases : string array;
      h_addrtype : socket_domain;
      h_addr_list : inet_addr array;
    }
    type protocol_entry =
      Unix.protocol_entry = {
      p_name : string;
      p_aliases : string array;
      p_proto : int;
    }
    type service_entry =
      Unix.service_entry = {
      s_name : string;
      s_aliases : string array;
      s_port : int;
      s_proto : string;
    }
    val gethostname : unit -> string
    val gethostbyname : string -> host_entry
    val gethostbyaddr : inet_addr -> host_entry
    val getprotobyname : string -> protocol_entry
    val getprotobynumber : int -> protocol_entry
    val getservbyname : string -> protocol:string -> service_entry
    val getservbyport : int -> protocol:string -> service_entry
    type addr_info =
      UnixLabels.addr_info = {
      ai_family : socket_domain;
      ai_socktype : socket_type;
      ai_protocol : int;
      ai_addr : sockaddr;
      ai_canonname : string;
    }
    type getaddrinfo_option =
      UnixLabels.getaddrinfo_option =
        AI_FAMILY of socket_domain
      | AI_SOCKTYPE of socket_type
      | AI_PROTOCOL of int
      | AI_NUMERICHOST
      | AI_CANONNAME
      | AI_PASSIVE
    val getaddrinfo :
      string -> string -> getaddrinfo_option list -> addr_info list
    type name_info =
      UnixLabels.name_info = {
      ni_hostname : string;
      ni_service : string;
    }
    type getnameinfo_option =
      UnixLabels.getnameinfo_option =
        NI_NOFQDN
      | NI_NUMERICHOST
      | NI_NAMEREQD
      | NI_NUMERICSERV
      | NI_DGRAM
    val getnameinfo : sockaddr -> getnameinfo_option list -> name_info
    type terminal_io =
      Unix.terminal_io = {
      mutable c_ignbrk : bool;
      mutable c_brkint : bool;
      mutable c_ignpar : bool;
      mutable c_parmrk : bool;
      mutable c_inpck : bool;
      mutable c_istrip : bool;
      mutable c_inlcr : bool;
      mutable c_igncr : bool;
      mutable c_icrnl : bool;
      mutable c_ixon : bool;
      mutable c_ixoff : bool;
      mutable c_opost : bool;
      mutable c_obaud : int;
      mutable c_ibaud : int;
      mutable c_csize : int;
      mutable c_cstopb : int;
      mutable c_cread : bool;
      mutable c_parenb : bool;
      mutable c_parodd : bool;
      mutable c_hupcl : bool;
      mutable c_clocal : bool;
      mutable c_isig : bool;
      mutable c_icanon : bool;
      mutable c_noflsh : bool;
      mutable c_echo : bool;
      mutable c_echoe : bool;
      mutable c_echok : bool;
      mutable c_echonl : bool;
      mutable c_vintr : char;
      mutable c_vquit : char;
      mutable c_verase : char;
      mutable c_vkill : char;
      mutable c_veof : char;
      mutable c_veol : char;
      mutable c_vmin : int;
      mutable c_vtime : int;
      mutable c_vstart : char;
      mutable c_vstop : char;
    }
    val tcgetattr : file_descr -> terminal_io
    type setattr_when = Unix.setattr_when = TCSANOW | TCSADRAIN | TCSAFLUSH
    val tcsetattr : file_descr -> mode:setattr_when -> terminal_io -> unit
    val tcsendbreak : file_descr -> duration:int -> unit
    val tcdrain : file_descr -> unit
    type flush_queue = Unix.flush_queue = TCIFLUSH | TCOFLUSH | TCIOFLUSH
    val tcflush : file_descr -> mode:flush_queue -> unit
    type flow_action = Unix.flow_action = TCOOFF | TCOON | TCIOFF | TCION
    val tcflow : file_descr -> mode:flow_action -> unit
    val setsid : unit -> int
  end
module Set :
  sig
    type 'a t = 'a PSet.Set.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
    val add : 'a -> 'a t -> 'a t
    val singleton : 'a -> 'a t
    val remove : 'a -> 'a t -> 'a t
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
    val subset : 'a t -> 'a t -> bool
    val iter : f:('a -> unit) -> 'a t -> unit
    val fold : f:('a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val partition : f:('a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val elements : 'a t -> 'a list
    val min_elt : 'a t -> 'a
    val max_elt : 'a t -> 'a
    val choose : 'a t -> 'a
    val of_list : 'a list -> 'a t
  end
module ZSet :
  sig
    type elt = ZZp.zz
    type t = ZZp.Set.t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : f:(elt -> unit) -> t -> unit
    val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
    val for_all : f:(elt -> bool) -> t -> bool
    val exists : f:(elt -> bool) -> t -> bool
    val filter : f:(elt -> bool) -> t -> t
    val partition : f:(elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
exception Bug of string
type key = Bitstring.t
module WHash :
  sig
    type data = key
    type t
    val create : int -> t
    val clear : t -> unit
    val merge : t -> data -> data
    val add : t -> data -> unit
    val remove : t -> data -> unit
    val find : t -> data -> data
    val find_all : t -> data -> data list
    val mem : t -> data -> bool
    val iter : (data -> unit) -> t -> unit
    val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
    val count : t -> int
    val stats : t -> int * int * int * int * int * int
  end
type writestatus = Clean | Dirty
type 'a disk = OnDisk of key | InMem of 'a
type children = Leaf of string Set.t | Children of node disk array
and node = {
  svalues : ZZp.mut_array;
  key : key;
  mutable num_elements : int;
  mutable children : children;
  mutable wstatus : writestatus;
}
type 'a db = {
  load : string -> string;
  save : 'a option -> key:string -> data:string -> unit;
  delete : 'a option -> string -> unit;
  create_txn : unit -> 'a option;
  commit_txn : 'a option -> unit;
  abort_txn : 'a option -> unit;
  mutable maxnodes : int;
  mutable inmem_count : int;
}
type 'a tree = {
  root : node;
  num_samples : int;
  split_thresh : int;
  join_thresh : int;
  bitquantum : int;
  points : ZZp.zz array;
  db : 'a db option;
  mutable synctime : float;
}
type dheader = {
  d_num_samples : int;
  d_split_thresh : int;
  d_join_thresh : int;
  d_bitquantum : int;
  d_points : ZZp.zz array;
}
val op_unwrap : 'a option -> 'a
val op_apply : f:('a -> 'b) -> 'a option -> 'b option
val op_map : f:('a -> 'b) -> 'a option list -> 'b option list
val child_keys_rec : Bitstring.t -> bit:int -> len:int -> Bitstring.t Set.t
val child_keys_raw : int -> Bitstring.t -> Bitstring.t list
val child_keys : 'a tree -> Bitstring.t -> Bitstring.t list
val marshal_to_string :
  f:(Channel.out_channel_obj -> 'a -> 'b) -> 'a -> string
val unmarshal_of_string : f:(Channel.in_channel_obj -> 'a) -> string -> 'a
val samesize : string Set.t -> bool
val marshal_node : Channel.out_channel_obj -> node -> unit
val unmarshal_node :
  bitquantum:int -> num_samples:int -> Channel.in_channel_obj -> node
val node_to_string : node -> string
val node_of_string_raw : bitquantum:int -> num_samples:int -> string -> node
val node_of_string : 'a tree -> string -> node
val marshal_header :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  'a tree -> unit
val unmarshal_dheader :
  < read_byte : int; read_char : char; read_float : float; read_int : 
    int; read_int32 : int32; read_int64 : int64;
    read_int64_size : int -> int64; read_int_size : int -> int;
    read_string : int -> string;
    read_string_pos : buf:string -> pos:int -> len:int -> unit;
    upcast : #Channel.in_channel_obj; .. > ->
  dheader
val header_to_string : 'a tree -> string
val dheader_of_string : string -> dheader
val dheader_to_header : 'a db option -> node -> dheader -> float -> 'a tree
val marshal_synctime : < write_float : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_synctime : < read_float : 'a; .. > -> 'a
val synctime_to_string : float -> string
val synctime_of_string : string -> float
val dbkey_of_key : Bitstring.t -> string
val int_to_bstring : int -> string
val root_dbkey : string
val header_dbkey : string
val synctime_dbkey : string
val load_node : 'a tree -> string -> node
val load_child : 'a tree -> node disk array -> int -> node
val load_child_sef : 'a tree -> node disk array -> int -> node
val save_node : 'a tree -> 'a option -> node -> unit
val save_synctime : 'a tree -> 'a option -> unit
val clean_subtree : 'a tree -> 'a option -> node -> unit
val clean : 'a option -> 'a tree -> unit
val delete_subtree_rec : 'a option -> 'a tree -> node disk -> unit
val delete_subtree : 'a option -> 'a tree -> node -> unit
val summarize_tree_rec :
  lagg:(string Set.t -> 'a) ->
  cagg:('a array -> 'a) -> 'b tree -> node disk -> 'a
val summarize_tree :
  lagg:(string Set.t -> 'a) -> cagg:('a array -> 'a) -> 'b tree -> 'a
val count_nodes : 'a tree -> int
val ( <+> ) : int * int -> int * int -> int * int
val count_node_types : 'a tree -> int * int
val get_elements : 'a tree -> node -> string Set.t
val get_zzp_elements : 'a tree -> node -> ZSet.t
val iter : f:(string -> unit) -> 'a tree -> unit
val count_inmem : node -> int
val count_inmem_tree : 'a tree -> int
val get_inmem_count : 'a tree -> int
val set_inmem_count : 'a tree -> int -> unit
val list_extract : f:('a -> 'b option) -> 'a list -> 'b list
val list_prefix : int -> 'a list -> 'a list
val list_prefix_suffix : int -> 'a list -> 'a list * 'a list
val inmem_children : node -> node list
val get_frontier :
  'a ->
  frontier:node list ->
  newfrontier:node list -> n:int -> count:int -> node list * int
val disconnect_children : node -> unit
val shrink_tree : 'a tree -> 'a option -> int -> unit
val shrink_tree_if_necessary : 'a tree -> 'a option -> unit
val width : int
val rmask : int -> int
val lmask : int -> int
val string_index : 'a tree -> int -> string -> int
val create_svalues : 'a array -> ZZp.mut_array
val incr_inmem_count : 'a tree -> unit
val decr_inmem_count : 'a tree -> unit
val create_node_basic : key -> 'a array -> node
val create_node : 'a tree -> key -> node
val add_to_node : 'a -> node -> 'b -> string -> ZZp.zz array -> unit
val remove_from_node : 'a -> node -> 'b -> string -> ZZp.zz array -> unit
val split_at_depth : 'a tree -> 'b -> 'c -> node -> int -> unit
val pad : string -> int -> string
val create_empty_header :
  points:ZZp.zz array ->
  bitquantum:int ->
  num_samples:int -> thresh:int -> dbopt:'a db option -> 'a tree
val create :
  ?db:(string -> string) * ('a option -> key:string -> data:string -> unit) *
      ('a option -> string -> unit) *
      ((unit -> 'a option) * ('a option -> unit) * ('a option -> unit)) * 
      int ->
  txn:'a option ->
  num_samples:int -> bitquantum:int -> thresh:int -> unit -> 'a tree
val insert_at_depth :
  'a tree -> 'b -> string -> node -> ZZp.zz array -> int -> unit
val insert_both : 'a tree -> 'a option -> ZZp.zz -> string -> unit
val insert : 'a tree -> 'a option -> ZZp.zz -> unit
val insert_str : 'a tree -> 'a option -> string -> unit
val get_ondisk_subkeys : 'a tree -> 'b db -> Bitstring.t -> Bitstring.t Set.t
val delete_at_depth :
  'a tree -> 'a option -> 'b -> string -> node -> ZZp.zz array -> int -> unit
val delete_both : 'a tree -> 'a option -> ZZp.zz -> string -> unit
val delete : 'a tree -> 'a option -> ZZp.zz -> unit
val delete_str : 'a tree -> 'a option -> string -> unit
val set_maxnodes : 'a tree -> 'a option -> int -> unit
val get_maxnodes : 'a tree -> int
val get_node_rec :
  sef:bool ->
  'a tree -> node -> string -> depth:int -> goal_depth:int -> node
val get_node_str : ?sef:bool -> 'a tree -> string -> int -> node
val get_node : ?sef:bool -> 'a tree -> ZZp.zz -> int -> node
val get_node_key : ?sef:bool -> 'a tree -> Bitstring.t -> node
val root : 'a tree -> node
val children : node -> node disk array option
val svalues : node -> ZZp.mut_array
val size : node -> int
val is_leaf : node -> bool
val points : 'a tree -> ZZp.zz array
val elements : 'a tree -> node -> ZSet.t
val node_size : 'a tree -> node disk -> int
val nonempty_children : 'a tree -> node disk array -> int list
val random_element : 'a list -> 'a
val get_random : 'a tree -> node -> string
val set_synctime : 'a tree -> float -> unit
val get_synctime : 'a tree -> float
val depth : 'a tree -> node -> int
val num_elements : 'a -> node -> int
