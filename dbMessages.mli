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
type configvar =
    [ `float of float | `int of int | `none | `string of string ]
val marshal_config :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string * [< `float of float | `int of int | `none | `string of string ] ->
  unit
val unmarshal_config :
  < read_byte : int; read_float : 'a; read_int : 'b; read_string : 'b -> 'c;
    .. > ->
  'c * [> `float of 'a | `int of 'b | `none | `string of 'c ]
type msg =
    WordQuery of string list
  | LogQuery of (int * Common.timestamp)
  | HashRequest of string list
  | LogResp of (Common.timestamp * Common.event) list
  | Keys of Packet.key list
  | KeyStrings of string list
  | Ack of int
  | MissingKeys of (string list * Unix.sockaddr)
  | Synchronize
  | RandomDrop of int
  | ProtocolError
  | DeleteKey of string
  | Config of (string * configvar)
  | Filters of string list
val marshal_timestamp : < write_float : 'a -> 'b; .. > -> 'a -> 'b
val unmarshal_timestamp : < read_float : 'a; .. > -> 'a
val marshal_logquery :
  < write_float : 'a -> 'b; write_int : 'c -> 'd; .. > -> 'c * 'a -> 'b
val unmarshal_logquery : < read_float : 'a; read_int : 'b; .. > -> 'b * 'a
val marshal_event :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Common.event -> unit
val unmarshal_event :
  < read_byte : int; read_int : 'a; read_string : 'a -> string; .. > ->
  Common.event
val marshal_log_entry :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  float * Common.event -> unit
val unmarshal_log_entry :
  < read_byte : int; read_float : 'a; read_int : 'b;
    read_string : 'b -> string; .. > ->
  'a * Common.event
val marshal_key :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Packet.packet list -> unit
val unmarshal_key :
  < read_int : 'a; read_string : 'a -> string; .. > -> Packet.packet list
val marshal_key_list :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  Packet.packet list list -> unit
val unmarshal_key_list :
  < read_int : int; read_string : int -> string; .. > ->
  Packet.packet list list
val marshal_missingkeys :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : float -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  string list * Unix.sockaddr -> unit
val unmarshal_missingkeys :
  < read_byte : int; read_int : int; read_string : int -> string; .. > ->
  string list * Unix.sockaddr
val marshal_msg :
  < upcast : #Channel.out_channel_obj; write_byte : int -> unit;
    write_char : char -> unit; write_float : Common.timestamp -> unit;
    write_int : int -> unit; write_int32 : int32 -> unit;
    write_int64 : int64 -> unit; write_string : string -> unit;
    write_string_pos : buf:string -> pos:int -> len:int -> unit; .. > ->
  msg -> unit
val unmarshal_msg :
  < read_byte : int; read_float : Common.timestamp; read_int : int;
    read_string : int -> string; .. > ->
  msg
val sockaddr_to_string : Unix.sockaddr -> string
val msg_to_string : msg -> string
module M :
  sig
    type msg_container = { msg : msg; }
    val marshal_noflush :
      < upcast : Channel.out_channel_obj; .. > -> msg -> unit
    val marshal :
      < flush : 'a; upcast : Channel.out_channel_obj; .. > -> msg -> 'a
    val unmarshal : < upcast : Channel.in_channel_obj; .. > -> msg_container
  end
type msg_container = M.msg_container = { msg : msg; }
val marshal_noflush : < upcast : Channel.out_channel_obj; .. > -> msg -> unit
val marshal :
  < flush : 'a; upcast : Channel.out_channel_obj; .. > -> msg -> 'a
val unmarshal : < upcast : Channel.in_channel_obj; .. > -> msg_container
