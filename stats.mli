val last : 'a list -> 'a
type histogram_entry = {
  upper : float;
  lower : float;
  mutable num_adds : int;
  mutable num_dels : int;
}
external get_tzname : unit -> string * string = "caml_get_tzname"
val time_to_tz_string : float -> string
val time_to_string : float -> string
val time_to_date : float -> string
val time_to_hour : float -> string
val round_up_to_day : float -> float
val round_up_to_hour : float -> float
val histogram_log :
  now:float -> float -> (float * Common.event) array -> histogram_entry array
val histogram_to_table : (float -> string) -> histogram_entry array -> string
val info_tables : unit -> string
val generate_html_stats_page : (float * Common.event) list -> int -> string
val generate_html_stats_page_nostats : unit -> string
