(***********************************************************************)
(* stats.ml - functions for formatting raw DB stats                    *)
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

open StdLabels
open MoreLabels
open Printf
open Common
open Packet
module Unix = UnixLabels


let rec last list = match list with
    [x] -> x | hd::tl -> last tl | _ -> raise Not_found

type histogram_entry =
    {
      upper: float;
      lower: float;
      mutable num_adds: int;
      mutable num_dels: int;
    }

(************************************************************)

external get_tzname : unit -> (string * string) = "caml_get_tzname"

let time_to_tz_string time =
  let tm = Unix.localtime time in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d %s"
    (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (fst (get_tzname ()))

let time_to_string time =
  let tm = Unix.localtime time in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let time_to_date time =
  let tm = Unix.localtime time in
  sprintf "%04d-%02d-%02d"
    (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday

let time_to_hour time =
  let tm = Unix.localtime time in
  sprintf "%04d-%02d-%02d %02d"
    (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour

(************************************************************)

let round_up_to_day time =
  let tm = Unix.localtime time in
  let tm = {tm with
              Unix.tm_hour = 24;
              Unix.tm_min = 0;
              Unix.tm_sec = 0;}
  in
  let (time,tm) = Unix.mktime tm in
  time

let round_up_to_hour time =
  let tm = Unix.localtime time in
  let tm = {tm with
              Unix.tm_min = 60;
              Unix.tm_sec = 0;}
  in
  let (time,tm) = Unix.mktime tm in
  time


(************************************************************)

let histogram_log ~now binsize log =
  let oldtime = fst log.(0) in
  let newtime = now in

  let nbins = truncate (ceil ((newtime -. oldtime) /. binsize)) in
  let bins = Array.init nbins
                ~f:(fun i -> {
                      upper = newtime -. binsize *. float i;
                      lower = newtime -. binsize *. float (i + 1);
                      num_adds = 0; num_dels = 0; } )
  in
  Array.iter log
    ~f:(fun (time,op) ->
          let bin_idx = truncate ((newtime -. time) /. binsize) in
          let bin = bins.(bin_idx) in
          if time < bin.lower || time > bin.upper
          then failwith "bad bin placement";
          match op with
              Add _ -> bin.num_adds <- bin.num_adds + 1
            | Delete _ -> bin.num_dels <- bin.num_dels + 1
       );
  bins

(************************************************************)

let histogram_to_table time_to_string histogram =
  let hist_entry_to_table_entry entry =
    sprintf "<tr><td>%s</td><td>%d</td><td>%d</td></tr>"
      (time_to_string entry.lower)
      (entry.num_adds - entry.num_dels) entry.num_dels
  in
  let table_entries =
    List.map ~f:hist_entry_to_table_entry (Array.to_list histogram)
  in
  "<table summary=\"Statistics\" border=\"1\">\n" ^
  "<tr><td>Time</td><td>New Keys</td><td>Updated Keys</td></tr>\n" ^
  String.concat "\n" table_entries ^
  "\n</table>\n"


(************************************************************)

let info_tables () =
  let settings =
    sprintf
      "<h2>Settings</h2>
     <table summary=\"Keyserver Settings\">
     <tr><td>Hostname:</td><td>%s</td></tr>
     <tr><td>Nodename:</td><td>%s</td></tr>
     <tr><td>Version:</td><td>%s%s</td></tr>
     <tr><td>Server contact:</td><td>%s</td></tr>
     <tr><td>HTTP port:</td><td>%d</td></tr>
     <tr><td>Recon port:</td><td>%d</td></tr>
     <tr><td>Debug level:</td><td>%d</td></tr>
</table>\r\n"
      !Settings.hostname !Settings.nodename Common.version Common.version_suffix
      !Settings.server_contact http_port recon_port !Settings.debuglevel
  in
  let gossip_peers =
    let peers = Array.to_list (Membership.get_names ()) in
    let peers = List.map ~f:(fun peer -> sprintf "<tr><td>%s</td></tr>\n" peer) peers in
    sprintf "<h2>Gossip Peers</h2>\n<table summary=\"Gossip Peers\">\n%s</table>"
      (String.concat ~sep:"" peers)
  in
  let mail_peers =
    let peers =
      try Membership.get_mailsync_partners ()
      with Failure "No partners specified" -> []
    in
    let peers = List.map ~f:(fun s -> sprintf "<tr><td>%s</td></tr>\n" s) peers in
    sprintf "<h2>Outgoing Mailsync Peers</h2>\n<table summary=\"Mailsync Peers\">\n%s</table>"
      (String.concat ~sep:"" peers)
  in
  sprintf "%s\n\n<table summary=\"Keyserver Peers\" width=\"100%%\">
<tr valign=\"top\"><td>
%s
</td><td>
%s
</td></tr></table>\r\n"
    settings gossip_peers mail_peers


(************************************************************)

let generate_html_stats_page log size =
  let log = Array.of_list log in
  let now = Unix.gettimeofday () in
  let num_keys = sprintf "<p>Total number of keys: %d</p>\n" size  in
  let title =
    sprintf
      "SKS OpenPGP Keyserver statistics<br />Taken at %s"
      (time_to_tz_string now)
  in
  if Array.length log = 0 then
    HtmlTemplates.page
      ~title
      ~body:(info_tables () ^ num_keys ^ "\n<p>No recent transactions</p>")
  else
    let last_time = fst log.(Array.length log - 1) in
    let daily_histogram = histogram_log (60. *. 60. *. 24.) log
                            ~now:(round_up_to_day last_time)
    and hourly_histogram = histogram_log (60. *. 60.) log
                             ~now:(round_up_to_hour last_time)
    in
    let daily_table = histogram_to_table time_to_date daily_histogram
    and hourly_table = histogram_to_table time_to_hour hourly_histogram
    in
    let body =
      info_tables () ^
      "<h2>Statistics</h2>" ^
      num_keys ^
      "<h3>Daily Histogram</h3>\n" ^
      daily_table ^
      "<h3>Hourly Histogram</h3>\n" ^
      hourly_table
    in
    HtmlTemplates.page ~title ~body


let generate_html_stats_page_nostats () =
  let body = info_tables () ^
             "<br /> Database statistics are time-consuming and so are " ^
             "only calculated once per day"
  in
  let title = "Stats not calculated yet" in
  HtmlTemplates.page ~title ~body


