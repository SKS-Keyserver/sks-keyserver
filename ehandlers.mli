val repeat_until :
  redo_timeout:float ->
  full_timeout:float ->
  test:(unit -> bool) ->
  init:(unit -> 'a) ->
  request:(unit -> Eventloop.timed_event list) ->
  success:(unit -> Eventloop.timed_event list) ->
  failure:(unit -> Eventloop.timed_event list) -> Eventloop.timed_event list
val float_incr : float -> float
val float_decr : float -> float
val strftime : float -> string
val repeat_forever :
  ?jitter:float ->
  ?start:float -> float -> Eventloop.callback -> Eventloop.timed_event list
val repeat_forever_simple :
  float -> (unit -> 'a) -> Eventloop.timed_event list
val incr_day : float -> float
val set_hour : float -> int -> float
val repeat_at_hour :
  int -> (unit -> Eventloop.timed_event list) -> Eventloop.timed_event list
