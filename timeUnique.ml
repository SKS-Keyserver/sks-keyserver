(* An interface to Unix.gettimeofday() which enforces that time always goes
 * up and never repeats. *)

let most_recent_unique_time : float ref  = ref 0.

(* gettimeofday() returns seconds & microseconds, so minimum meaningful
 * increment is 1 microsecond; OCaml uses IEEE 754 double-precision floats,
 * which gives 53 bits of mantissa.  Assuming 32 bits for time until 32-bit
 * time_t overflows, we can knock bits off 21 bits depending upon when we want
 * the overflow/rollover to occur, and whatever's left is available for delta
 * even at the end of the lifetime of the code; as that fateful day approaches,
 * lower the granularity of this delay accordly  *)

(* we don't use epsilon_float, as that's only guaranteed to give a different
 * result when added to 1.0, not for other numbers. *)

let timestamp_delta = 0.000001

(* If wallclock time goes backwards, we won't, but time will appear to go
 * forward very very slowly until wallclock catches back up *)

let getuniquetimeofday nil =
  let candidate = Unix.gettimeofday() in
  let final =
    match candidate > !most_recent_unique_time with
      true -> candidate
    | false -> !most_recent_unique_time +. timestamp_delta
  in
  most_recent_unique_time := final;
  final
