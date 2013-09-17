(***********************************************************************)
(* htmlTemplates.ml                                                    *)
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

open Printf
open StdLabels
open MoreLabels
module Unix = UnixLabels
open Unix

open Packet

let html_quote string =
  let sin = new Channel.string_in_channel string 0 in
  let sout = Channel.new_buffer_outc (String.length string + 10) in
  try
    while true do
      match sin#read_char with
        | '<' -> sout#write_string "&lt;"
        | '>' -> sout#write_string "&gt;"
        | '&' -> sout#write_string "&amp;"
        | '"' -> sout#write_string "&quot;"
        | '\''-> sout#write_string "&#x27;"
        | '/'-> sout#write_string "&#x2F;"
        | c -> sout#write_char c
    done;
    ""
  with
      End_of_file ->
        sout#contents

let br_regexp = Str.regexp_case_fold "<br />"
let page ~title ~body =
  sprintf
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\r\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" >\r\n<html xmlns=\"http://www.w3.org/1999/xhtml\">\r\n<head>\r\n<title>%s</title>\r\n<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />\r\n<style type=\"text/css\">\r\n/*<![CDATA[*/\r\n .uid { color: green; text-decoration: underline; }\r\n .warn { color: red; font-weight: bold; }\r\n/*]]>*/\r\n</style></head><body><h1>%s</h1>%s</body></html>"
    (Str.global_replace br_regexp  "&nbsp;|&nbsp;" title) title body

let link ~op ~hash ~fingerprint ~keyid =
  sprintf "/pks/lookup?op=%s%s%s&amp;search=0x%s"
    op
    (if hash then "&amp;hash=on" else "")
    (if fingerprint then "&amp;fingerprint=on" else "")
    keyid

let keyinfo_header = "Type bits/keyID     Date       User ID"

let keyinfo_pks pki revoked ~keyid ~link ~userids =
  let tm = gmtime (Int64.to_float pki.pk_ctime) in
  let algo = pk_alg_to_ident pki.pk_alg in
  let base =
    sprintf "pub  %4d%s/<a href=\"%s\">%8s</a> %4d-%02d-%02d%s "
      pki.pk_keylen algo link keyid
      (1900 + tm.tm_year)
      (tm.tm_mon + 1)
      tm.tm_mday
      (if revoked then " *** KEY REVOKED *** [not verified]\r\n                              "
       else "")
  in
  let uidstr = String.concat ~sep:"\r\n                               " userids in
  base ^ uidstr

let fingerprint ~fp =
  sprintf "\t Fingerprint=%s" fp

let hash_link ~hash =
  sprintf "/pks/lookup?op=hget&amp;search=%s" hash

let hash ~hash =
  sprintf "\t Hash=<a href=%s>%s</a>" (hash_link ~hash) hash

let preformat_list elements =
  sprintf "<pre>%s</pre>"
    (String.concat ~sep:"\r\n" elements ^ "\r\n")
