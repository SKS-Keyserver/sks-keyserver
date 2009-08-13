(** Module for tracking gossip membership and mailsync peers *)

(** Reset the last time the mtime was read to zero, to force the membership file to be
    reloaded from disk *)
val reset_membership_time : unit -> unit

(** Get human-readable names of gossip peers. *)
val get_names : unit -> string array

(** Picks single gossip partner from list of possible partners, and returns list of all
    known addresses for that host *)
val choose : unit -> Common.Unix.addr_info list

(** Returns true iff the address in question belongs to one of the hosts on the gossip
    membership list. *)
val test : Common.Unix.sockaddr -> bool

(** Returns the list of email addresses for use in PKS-style key distribution *)
val get_mailsync_partners : unit -> string list
  
