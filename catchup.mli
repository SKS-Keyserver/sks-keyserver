val last_ts : ('a * 'b) list -> 'a
val event_to_hash : Common.event -> string
val sortlog : ('a * Common.event) list -> ('a * Common.event) list
val combine : f:('a -> 'a -> 'a) -> 'a list -> 'a
val max_timestamp : ('a * 'b) list -> 'a
val applylog : Bdb.Txn.t option -> (float * Common.event) list -> unit
val single_catchup : int -> bool
val count : int
val uninterruptable_catchup : unit -> unit
val catchup : unit -> Eventloop.timed_event list
val catchup_interval : float
