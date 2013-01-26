(***********************************************************************)
(* index.ml - code for generating pretty PGP key indices               *)
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
open Request
open Pstyle

module Map = PMap.Map

(********************************************************************)

type siginfo = { mutable userid: string option;
                 mutable policy_url: string option;
                 mutable notation_data: (string * string) option;
                 mutable revocation_key: string option;
                 mutable is_primary_uid: bool;
                 mutable keyid: string option;
                 mutable sigtype: int;
                 mutable sig_creation_time: int64 option;
                 mutable sig_expiration_time: int64 option;
                 mutable key_expiration_time: int64 option;
               }

(********************************************************************)

let empty_siginfo () =
  { userid = None;
    policy_url = None;
    notation_data = None;
    revocation_key = None;
    is_primary_uid = false;
    keyid = None;
    sigtype = 0;
    sig_creation_time = None;
    sig_expiration_time = None;
    key_expiration_time = None;
  }

(********************************************************************)

let keyinfo_header request =
  if request.kind = VIndex then
    "Type bits/keyID     cr. time   exp time   key expir"
  else
    HtmlTemplates.keyinfo_header

(********************************************************************)

let sig_to_siginfo sign =
  let siginfo = empty_siginfo () in
  begin
    match ParsePGP.parse_signature sign with
      | V3sig s ->
          siginfo.sigtype <- s.v3s_sigtype;
          siginfo.keyid <- Some s.v3s_keyid;
          siginfo.sig_creation_time <- Some s.v3s_ctime
      | V4sig s ->
          let update_siginfo ssp =
            match ssp.ssp_type with

              | 2 -> (* sign. expiration time *)
                  if ssp.ssp_length = 4 then
                    siginfo.sig_creation_time <-
                    Some (ParsePGP.int64_of_string ssp.ssp_body)

              | 3 -> (* sign. expiration time *)
                  if ssp.ssp_length = 4 then
                    siginfo.sig_expiration_time <-
                    let exp = ParsePGP.int64_of_string ssp.ssp_body in
                    if Int64.compare exp Int64.zero = 0
                    then None else Some exp

              | 9 -> (* key expiration time *)
                  if ssp.ssp_length = 4 then
                    siginfo.key_expiration_time <-
                    let exp = ParsePGP.int64_of_string ssp.ssp_body in
                    if Int64.compare exp Int64.zero = 0
                    then None else Some exp

              | 12 -> (* revocation key *)
                  let cin = new Channel.string_in_channel ssp.ssp_body 0 in
                  let _revclass = cin#read_int_size 1 in
                  let _algid = cin#read_int_size 1 in
                  let fingerprint = cin#read_string 20 in
                  siginfo.revocation_key <- Some fingerprint

              | 16 -> (* issuer keyid *)
                  if ssp.ssp_length = 8 then
                    siginfo.keyid <- Some ssp.ssp_body
                  else
                    printf "Argh!  that makes no sense: %d\n" ssp.ssp_length

              | 20 -> (* notation data *)
                  let cin = new Channel.string_in_channel ssp.ssp_body 0 in
                  let flags = cin#read_string 4 in
                  let name_len = cin#read_int_size 2 in
                  let value_len = cin#read_int_size 2 in
                  let name_data = cin#read_string name_len in
                  let value_data = cin#read_string value_len in

                  if Char.code flags.[0] = 0x80 then
                    (* human-readable notation data *)
                    siginfo.notation_data <- Some (name_data,value_data)

              | 25 -> (* primary userid (bool) *)
                  if ssp.ssp_length = 1 then
                    let v = int_of_char ssp.ssp_body.[0] in
                    siginfo.is_primary_uid <- v <> 0

              | 26 -> (* policy URL *)
                  siginfo.policy_url <- Some ssp.ssp_body

              | 28 -> (* signer's userid *)
                  siginfo.userid <- Some ssp.ssp_body

              | _ -> (* miscellaneous other packet *)
                  ()
          in
          siginfo.sigtype <- s.v4s_sigtype;
          List.iter (s.v4s_hashed_subpackets @ s.v4s_unhashed_subpackets)
            ~f:(fun ssp -> try update_siginfo ssp with End_of_file -> ())
  end;
  siginfo

(********************************************************************)

(** sort signatures in ascending time order *)
let sort_siginfo_list list =
  List.stable_sort list
    ~cmp:(fun x y -> compare x.sig_creation_time y.sig_creation_time)

(********************************************************************)

let is_selfsig ~keyid siginfo = siginfo.keyid = Some keyid

(********************************************************************)

let is_primary ~keyid (uid,siginfo_list) =
  List.exists ~f:(fun siginfo ->
                    is_selfsig ~keyid siginfo
                    && siginfo.is_primary_uid
                    && uid.packet_type = User_ID_Packet
                 )
    siginfo_list

(********************************************************************)

(** returns time of most recent self-sig on uid *)
let max_selfsig_time ~keyid (uid,siginfo_list) =
  let selfsigs = List.filter ~f:(fun si -> is_selfsig ~keyid si)
                   siginfo_list in
  let times = filter_opts
                (List.map selfsigs
                   ~f:(function x -> match x.sig_creation_time with
                           None -> None
                         | Some time -> Some (Int64.to_float time)))
  in
  List.fold_left ~init:min_float ~f:max times

(********************************************************************)

let split_list ~f l =
  let rec loop l a b = match l with
      [] -> (List.rev a, List.rev b)
    | hd::tl ->
        if f hd then loop tl (hd::a) b
        else loop tl a (hd::b)
  in
  loop l [] []

(********************************************************************)

let move_primary_to_front ~keyid uids =
  let (primary,normal) = split_list ~f:(is_primary ~keyid) uids in
  let primary = List.stable_sort primary
               ~cmp:(fun x y -> compare
                       (max_selfsig_time ~keyid y)
                       (max_selfsig_time ~keyid x)
                    )
  in
  primary @ normal

(********************************************************************)

let convert_sigpair (uid,sigs) =
  (uid,List.map ~f:sig_to_siginfo sigs)

(********************************************************************)

let blank_datestr = "__________"
let no_datestr =    "          "
let datestr_of_int64 i =
  let tm = Unix.gmtime (Int64.to_float i) in
  sprintf "%04d-%02d-%02d" (1900 + tm.Unix.tm_year) (1 + tm.Unix.tm_mon)
    (tm.Unix.tm_mday)

(********************************************************************)

let siginfo_to_lines ~get_uid ?key_creation_time request self_keyid today siginfo =

  let sig_creation_string = match siginfo.sig_creation_time with
    | None -> blank_datestr
    | Some time -> datestr_of_int64 time
  in

  let key_expiration_string =
    match (key_creation_time,
           siginfo.key_expiration_time)
    with
    | (None,_) | (_,None) -> blank_datestr
    | (Some x,Some y) -> datestr_of_int64 (Int64.add x y)
  in

  let sig_expiration_string =
    match (siginfo.sig_creation_time,
           siginfo.sig_expiration_time)
    with
    | (None,_) | (_,None) -> blank_datestr
    | (Some x,Some y) -> datestr_of_int64 (Int64.add x y)
  in

  let sig_expired =
    match (siginfo.sig_creation_time,
           siginfo.sig_expiration_time)
    with
    | (None,_) | (_,None) -> false
    | (Some x,Some y) -> (Int64.to_float (Int64.add x y)) < today
  in

  let sigtype_string =
    match siginfo.sigtype with
      | 0x10 ->
         if sig_expired then "<span class=\"warn\"> exp  </span>"
         else " sig  "
      | 0x11 ->
         if sig_expired then "<span class=\"warn\"> exp1 </span>"
         else " sig1 "
      | 0x12 ->
         if sig_expired then "<span class=\"warn\"> exp2 </span>"
         else " sig2 "
      | 0x13 ->
         if sig_expired then "<span class=\"warn\"> exp3 </span>"
         else " sig3 "
      | 0x20 | 0x28 | 0x30 -> "<span class=\"warn\">revok </span>"
      | 0x1f -> "dirct "
      | 0x18 -> "sbind "
      | x -> sprintf " 0x%02x" x
  in

  let uid_string = match siginfo.userid with
    | Some s -> s
    | None ->
        if Some self_keyid = siginfo.keyid then "[selfsig]"
        else
          match apply_opt get_uid siginfo.keyid with
            | None | Some None -> "[]"
            | Some (Some uid) -> uid
  in
  let uid_string = HtmlTemplates.html_quote uid_string in
  let uid_string = match siginfo.keyid with
      None -> uid_string
    | Some keyid ->
        if uid_string = "" then ""
        else
          let long = Fingerprint.keyid_to_string ~short:false keyid in
          let link =
            HtmlTemplates.link ~op:"vindex"
              ~hash:request.hash ~fingerprint:request.fingerprint ~keyid:long
          in
          sprintf "<a href=\"%s\">%s</a>" link uid_string
  in

  let keyid_string = match siginfo.keyid with
    | Some keyid ->
        let short = Fingerprint.keyid_to_string ~short:true keyid in
        let long = Fingerprint.keyid_to_string ~short:false keyid in
        let link =
          HtmlTemplates.link ~op:"get"
            ~hash:request.hash ~fingerprint:request.fingerprint ~keyid:long
        in
        sprintf "<a href=\"%s\">%s</a>" link short
    | None ->
        "no keyid"
  in

  let firstline = sprintf "sig %-6s %s %s %s %s %s"
                    sigtype_string keyid_string
                    sig_creation_string sig_expiration_string
                    key_expiration_string
                    uid_string
  in

  let policy_url_opt =
    apply_opt siginfo.policy_url
      ~f:(fun policy_url ->
            let policy_url = HtmlTemplates.html_quote policy_url in
            sprintf "    Policy URL: <a href=\"%s\">%s</a>"
              policy_url policy_url
         )
  in
  let notation_data_opt =
    apply_opt siginfo.notation_data
      ~f:(fun (name,value) ->
              sprintf "    Notation data: <span class=\"text-decoration: underline;\">%s</span> %s"
            (HtmlTemplates.html_quote name)
            (HtmlTemplates.html_quote value)
         )
  in
  let revocation_key_opt =
    apply_opt siginfo.revocation_key
      ~f:(fun fingerprint ->
            sprintf "    Revocation key fingerprint: <a href=\"%s\">%s</a>"
            (HtmlTemplates.link ~hash:request.hash ~op:"vindex"
               ~fingerprint:request.fingerprint
               ~keyid:(Utils.hexstring fingerprint)
            )
            (Fingerprint.fp_to_string fingerprint)
         )
  in
  firstline :: filter_opts [policy_url_opt; notation_data_opt;
                            revocation_key_opt]


(********************************************************************)

let selfsigs_to_lines request key_creation_time keyid selfsigs today =
  let lines =
    List.map ~f:(fun sign -> siginfo_to_lines ~get_uid:(fun _ -> None)
                   ~key_creation_time request keyid today
                   (sig_to_siginfo sign))
      selfsigs
  in
  List.concat lines

(********************************************************************)

let uid_to_lines ~get_uid request key_creation_time keyid today
  (uid,siginfo_list) =
  let siginfo_list = sort_siginfo_list siginfo_list in
  let uid_line = match uid.packet_type with
    | User_ID_Packet ->
        sprintf "<strong>uid</strong> <span class=\"uid\">%s</span>"
        (HtmlTemplates.html_quote uid.packet_body)

    | _ -> sprintf "<strong>uat</strong> [contents omitted]"
  in
  let siginfo_lines =
    List.concat
      (List.map ~f:(siginfo_to_lines ~get_uid ~key_creation_time
                    request keyid today)
         siginfo_list)
  in
  ""::uid_line::siginfo_lines

let uids_to_lines ~get_uid request key_creation_time keyid uids today =
  List.concat
    (List.map ~f:(uid_to_lines ~get_uid request key_creation_time keyid today) uids)

(********************************************************************)

let key_packet_to_line ~is_subkey pki keyid =
  let prefix = if is_subkey then "<strong>sub</strong>" else "<strong>pub</strong>" in
  let creation_string = datestr_of_int64 pki.pk_ctime in
  let expiration_string =
    if pki.pk_version = 4 then no_datestr
    else
      match pki.pk_expiration with
        | None -> blank_datestr
        | Some days ->
            let time = Int64.add (Int64.of_int (days * 24 * 60 * 60))
                         pki.pk_ctime in
            datestr_of_int64 time
  in
  let keyid = keyid in
  let keyid_short = Fingerprint.keyid_to_string ~short:true keyid in
  let keyid_long = Fingerprint.keyid_to_string ~short:false keyid in

  let keyid_string =
    if is_subkey then sprintf "%8s" keyid_short
    else
      sprintf "<a href=\"%s\">%8s</a>"
        (HtmlTemplates.link ~op:"get" ~hash:false ~fingerprint:false
           ~keyid:keyid_long )
        keyid_short
  in
  let algo = pk_alg_to_ident pki.pk_alg in
  let line = sprintf "%s  %4d%s/%s %s %s "
               prefix
               pki.pk_keylen algo
               keyid_string
               creation_string expiration_string
  in
  (line,keyid)

(********************************************************************)

let subkey_to_lines request today (subkey,siginfo_list) =
  let pki = ParsePGP.parse_pubkey_info subkey in
  let keyid = (Fingerprint.from_packet subkey).Fingerprint.keyid in
  let (subkey_line,keyid) = key_packet_to_line ~is_subkey:true pki keyid in
  let key_creation_time = pki.pk_ctime in
  let siginfo_lines =
    List.concat (List.map ~f:(siginfo_to_lines ~get_uid:(fun _ -> None)
                                ~key_creation_time request keyid today)
                   siginfo_list)
  in
  ""::subkey_line::siginfo_lines

let subkeys_to_lines request subkeys today =
  List.concat (List.map ~f:(subkey_to_lines request today) subkeys)

(********************************************************************)
(* new style verbose key index **************************************)
(********************************************************************)

(** if f is true for any element of list, then return (Some x,newlist), where
  x is one such element, and newlist is list with x removed.  Otherwise,
  return (None,list)
*)
let rec extract ~f list = match list with
    [] -> (None,[])
  | hd::tl ->
      if f hd then (Some hd,tl)
      else let (x,new_tl) =  extract ~f tl in (x,hd::new_tl)

(** if there is an element in list for which f returns true, then return list
  with one such element moved to the front. *)
let move_to_front ~f list =
  match extract ~f list with
    | (None,list) -> list
    | (Some x,list) -> x::list

(********************************************************************)

(** fetches UID from keyid, stopping fater first [max_uid_fetches] *)
let get_uid get_uids =
  let ctr = ref 0 in
  (fun keyid ->
     try
       incr ctr;
       if !ctr > !Settings.max_uid_fetches then None
       else
         let uids = get_uids keyid in
         let uids = List.filter uids
                      ~f:(fun (uid,_) -> uid.packet_type = User_ID_Packet) in
         let uids = List.map ~f:convert_sigpair uids in
         match move_primary_to_front ~keyid uids with
           | [] -> None
           | (uid,_)::tl -> Some uid.packet_body
     with
       | e ->
           eplerror 3 e
             "Error fetching uid during VIndex for keyid 0x%s"
             (KeyHash.hexify keyid);
           None
  )

(********************************************************************)

(** computes fingerprint and hash lines if required *)
let get_extra_lines request key hash meta =

  let extra_lines =
    if request.fingerprint then
      [HtmlTemplates.fingerprint ~fp:(Fingerprint.fp_to_string
                                        meta.Fingerprint.fp)]
    else []
  in

  let extra_lines =
    if request.hash then
      let hash_line = HtmlTemplates.hash ~hash:(KeyHash.hexify hash) in
      hash_line::extra_lines
    else
      extra_lines
  in

  extra_lines

(********************************************************************)

(** computes key to verbose set of lines.  Note that these lines should be
  embedded inside of a <pre></pre> environment *)
let key_to_lines_verbose ~get_uids request key hash =
  try
    let get_uid = get_uid get_uids in
    let pkey = KeyMerge.key_to_pkey key in
    let selfsigs = pkey.KeyMerge.selfsigs
    and uids = List.map ~f:convert_sigpair pkey.KeyMerge.uids
    and subkeys = List.map ~f:convert_sigpair pkey.KeyMerge.subkeys
    and pubkey = pkey.KeyMerge.key in

    (* sort subkeys by creation time in ascending order *)
    let subkeys =
      List.map ~f:(fun (uid,siginfo) ->
                     (uid,sort_siginfo_list siginfo)) subkeys
    in

    let pki = ParsePGP.parse_pubkey_info pubkey in
    let meta = Fingerprint.from_packet pubkey in
    let keyid = meta.Fingerprint.keyid in
    let key_creation_time = pki.pk_ctime in

    let today = Stats.round_up_to_day (Unix.gettimeofday ()) in


    (** move primary keyid to front of the list *)
    let uids = move_primary_to_front ~keyid uids in

    (* let primary_uid_string = (fst (List.hd uids)).packet_body in *)
    let (pubkey_line,keyid) =
      key_packet_to_line ~is_subkey:false pki keyid in

    let extra_lines = get_extra_lines request key hash meta in

    (* note: ugly hack here. </pre> and <pre> are used to allow for an <hr>
       inside of a pre-formatted region.  So this code only works if the
       lines are being generated to be put inside of a <pre></pre> block> *)
    ("</pre><hr /><pre>" ^ pubkey_line) ::
    List.concat [
      selfsigs_to_lines request key_creation_time keyid selfsigs today;
      extra_lines;
      uids_to_lines ~get_uid request key_creation_time keyid uids today;
      subkeys_to_lines request subkeys today;
    ]

  with
    | Sys.Break | Eventloop.SigAlarm as e -> raise e
    | e ->
        eplerror 2 e
          "Unable to print key from query '%s'"
          (String.concat ~sep:" " request.search);
        []


(********************************************************************)
(* old style key index **********************************************)
(********************************************************************)

let sig_is_revok siginfo =
  match siginfo.sigtype with
    | 0x20 | 0x28 | 0x30 -> true
    | _ -> false

let is_revoked key =
  let pkey = KeyMerge.key_to_pkey key in
  let selfsigs = pkey.KeyMerge.selfsigs in
  List.exists ~f:(fun sign ->
                   sig_is_revok (sig_to_siginfo sign)
                 )
    selfsigs

(** oldstyle index lines *)
let key_to_lines_normal request key hash =
  try
    let pkey = KeyMerge.key_to_pkey key in
    let uids = List.map ~f:convert_sigpair pkey.KeyMerge.uids in

    let meta = Fingerprint.from_key key in
    let keyid = meta.Fingerprint.keyid in
    let keyid_short = Fingerprint.keyid_to_string ~short:true keyid in
    let keyid_long = Fingerprint.keyid_to_string ~short:false keyid in
    let link = HtmlTemplates.link ~op:"get" ~hash:false ~fingerprint:false
                 ~keyid:keyid_long in
    let ilink = HtmlTemplates.link ~op:"vindex"
                  ~hash:request.hash ~fingerprint:request.fingerprint
                  ~keyid:keyid_long in

    let uids = move_primary_to_front ~keyid uids in

    let userids =
      List.map ~f:(fun (uid,sigs) ->
                     match uid.packet_type with
                         User_ID_Packet ->
                           HtmlTemplates.html_quote uid.packet_body
                       | User_Attribute_Packet -> "[user attribute packet]"
                       | _ -> "[unexpected packet type]"
                  )
        uids
    in
    let userids = match userids with [] -> []
      | hd::tl -> (sprintf "<a href=\"%s\">%s</a>" ilink hd)::tl in
    let pki = ParsePGP.parse_pubkey_info (List.hd key) in
    let keystr = HtmlTemplates.keyinfo_pks pki (is_revoked key)
                    ~keyid:keyid_short ~link ~userids in
    let lines = [] in
    let lines =
      if request.fingerprint then
        let fingerprint = HtmlTemplates.fingerprint
                            ~fp:(Fingerprint.fp_to_string
                                   (meta.Fingerprint.fp))
        in
        fingerprint::lines
      else
        lines
    in
    let lines =
      if request.hash then
        let hash = HtmlTemplates.hash ~hash:(KeyHash.hexify hash) in
        hash::lines
      else
        lines
    in
    let lines =
        keystr::lines
    in
    "</pre><hr /><pre>"::lines
  with
    | Sys.Break | Eventloop.SigAlarm as e -> raise e
    | e ->
        eplerror 2 e
          "Unable to print key from query '%s'"
          (String.concat ~sep:" " request.search);
        []


