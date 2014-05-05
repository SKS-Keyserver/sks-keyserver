(***********************************************************************)
(* wserver.ml - simple web server code                                 *)
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
module Unix = UnixLabels
open Unix

module Map = PMap.Map
module Set = PSet.Set

exception Page_not_found of string
exception No_results of string
exception Not_implemented of string
exception Bad_request of string
exception Entity_too_large of string
exception Misc_error of string

let ( |= ) map key = Map.find key map
let ( |< ) map (key,data) = Map.add ~key ~data map

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)

let hexa_val conf =
  match conf with
    '0'..'9' -> Char.code conf - Char.code '0'
  | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
  | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
  | _ -> 0

let decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
        '%' | '+' -> true
      | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s -> i + 3
        | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          '%' when i + 2 < String.length s ->
            let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
            s1.[i1] <- Char.chr v; i + 3
        | '+' -> s1.[i1] <- ' '; succ i
        | x -> s1.[i1] <- x; succ i
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else s
    else s
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = String.create len in
    strip_heading_and_trailing_spaces (copy_decode_in s1 0 0)
  else s


let special x = List.mem x ['='; '&'; '"'; '\r'; '\n'; '+']

let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        ' ' -> true
      | x -> if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          ' ' -> s1.[i1] <- '+'; succ i1
        | c ->
            if special c then
              begin
                s1.[i1] <- '%';
                s1.[i1 + 1] <- hexa_digit (Char.code c / 16);
                s1.[i1 + 2] <- hexa_digit (Char.code c mod 16);
                i1 + 3
              end
            else begin s1.[i1] <- c; succ i1 end
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s

let stripchars = Set.of_list [ ' '; '\t'; '\n'; '\r' ]

let strip s =
  let start = ref 0 in
  while (!start < String.length s
         && Set.mem s.[!start] stripchars) do
    incr start
  done;
  let stop = ref (String.length s - 1) in
  while (!stop >= 0 && Set.mem s.[!stop] stripchars) do
    decr stop
  done;
  if !stop >= !start then
    String.sub s ~pos:!start ~len:(!stop - !start + 1)
  else
    ""


type 'a request = | GET of (string * (string,string) Map.t)
                  | POST of (string * (string,string) Map.t * 'a)

let whitespace = Str.regexp "[ \t\n\r]+"
let eol = Str.regexp "\r?\n"

let get_all cin =
  let buf = Buffer.create 0 in
  (try Buffer.add_channel buf cin 10000
   with End_of_file -> ());
  Buffer.contents buf

let get_lines cin =
  Str.split eol (get_all cin)

let max_post_length = 5 * 1024 * 1024  (* posts restricted to 5 Megs or less *)

let parse_post headers cin =
  try
    let lengthstr = headers |= "content-length" in
    let len = int_of_string lengthstr in
    if len > max_post_length
    then raise (Entity_too_large (sprintf "POST data too long: %f megs"
                              (float len /. 1024. /. 1024.)));
    let rest = String.create len in
    really_input cin rest 0 len;
    rest
  with
      Not_found ->
        failwith "parse_post failed for lack of a content-length header"

let is_blank line =
  String.length line = 0 || line.[0] = '\r'

let rec parse_headers map cin =
  let line = input_line cin in (* DoS attack: input_line is unsafe on sockets *)
  if is_blank line then map
  else
    let colonpos = try String.index line ':' with
        Not_found -> failwith "Error parsing headers: no colon found"
    in
    let key = String.sub line ~pos:0 ~len:colonpos
    and data = String.sub line ~pos:(colonpos + 1)
                 ~len:(String.length line - colonpos - 1)
    in
    parse_headers (map |< (String.lowercase key, strip data)) cin

let parse_request cin =
  let line = input_line cin in (* DoS attack: input_line is unsafe on sockets *)
  let pieces = Str.split whitespace line in
  let headers = parse_headers Map.empty cin in
  match List.hd pieces with
      "GET" -> GET (List.nth pieces 1,headers)
    | "POST" -> POST (List.nth pieces 1,headers,
                      parse_post headers cin)
    | _ -> failwith "Malformed header"

let headers_to_string map =
  let pieces = List.map ~f:(fun (x,y) -> sprintf "%s:%s" x y)
                 (Map.to_alist map)
  in
  "\n" ^ (String.concat "\n" pieces)

let request_to_string request =
  let (kind,req,headers) =
    match request with
      | GET (req,header_map) ->
          ("GET",req,headers_to_string header_map)
      | POST (req,header_map,_) ->
          ("POST",req,headers_to_string header_map)
  in
  sprintf "(%s,%s,[%s])" kind req headers

let request_to_string_short request =
  let (kind,request) =
    match request with
      | GET (req,header_map) ->
          ("GET",req)
      | POST (req,header_map,_) ->
          ("POST",req)
  in
  sprintf "(%s %s)" kind request

let request_to_string_logdepend request =
  if !Settings.debuglevel < 6 then
    request_to_string_short request
    else request_to_string request


(* Result codes and descriptions from                                               *)
(* https://support.google.com/webmasters/bin/answer.py?hl=en&answer=40132           *)
(* send_result exposes a completely open CORS policy, so use only with public data. *)

let send_result cout ?(error_code = 200) ?(content_type = "text/html; charset=UTF-8") ?(count = -1) body =
  let text_status =
    match error_code with
      | 200 -> "OK"
      | 201 -> "Created"
      | 202 -> "Accepted"
      | 203 -> "Non-Authoritative Information"
      | 204 -> "No Content"
      | 205 -> "Reset Content"
      | 206 -> "Partial Content"
      | 300 -> "Multiple Choices"
      | 301 -> "Moved Permanently"
      | 302 -> "Moved Temporarily"
      | 303 -> "See Other Location"
      | 304 -> "Not Modified"
      | 305 -> "Use Proxy"
      | 307 -> "Temporary Redirect"
      | 400 -> "Bad Request"
      | 401 -> "Not Authorized"
      | 403 -> "Forbidden"
      | 404 -> "Not found"
      | 405 -> "Method Not Allowed"
      | 406 -> "Not Acceptable"
      | 407 -> "Proxy Authentication Required"
      | 408 -> "Request Timeout"
      | 409 -> "Conflict"
      | 410 -> "Gone"
      | 411 -> "Length Required"
      | 412 -> "Precondition Failed"
      | 413 -> "Request Entity too Large"
      | 414 -> "Requested URI too Large"
      | 415 -> "Unsupported Media Type"
      | 416 -> "Requested Range not Satisfiable"
      | 417 -> "Expectation Failed"
      | 500 -> "Internal Server Error"
      | 501 -> "Not Implemented"
      | 502 -> "Bad Gateway"
      | 503 -> "Service Unavailable"
      | 504 -> "Gateway Timeout"
      | 505 -> "HTTP Version Not Supported"
      | _   -> "???"
  in
  fprintf cout "HTTP/1.0 %03d %s\r\n" error_code text_status;
  fprintf cout "Server: sks_www/%s%s\r\n" version version_suffix;
  fprintf cout "Cache-Control: no-cache\r\n";
  fprintf cout "Pragma: no-cache\r\n";
  fprintf cout "Expires: 0\r\n";
  fprintf cout "Content-length: %u\r\n" (String.length body + 2);
  if count >= 0 then
    fprintf cout "X-HKP-Results-Count: %d\r\n" count;
  fprintf cout "Content-type: %s\r\n" content_type;
  (*
   * Hack to force content-disposition for machine readable get request.
   * This should probably be passed down in the request itself.
   *)
  if content_type = "application/pgp-keys; charset=UTF-8" then
    fprintf cout "Content-disposition: attachment; filename=gpgkey.asc\r\n";
  (*
   * Allow access from Javascript code on other sites.
   * For details, see https://en.wikipedia.org/wiki/Cross-origin_resource_sharing.
   * This is safe since all information on keyservers is public.
   *)
  fprintf cout "Access-Control-Allow-Origin: *\r\n";
  (*
   * End Headers here with a final newline
   *)
  fprintf cout "\r\n";
  fprintf cout "%s\r\n" body;
  flush cout

let accept_connection f ~recover_timeout addr cin cout =
  begin
    try
      let request = parse_request cin in
      let output_chan = Channel.new_buffer_outc 0 in
      try
        let (content_type, count) = f addr request output_chan#upcast in
        let output = output_chan#contents in
        send_result cout ~content_type ~count output
      with
        | Eventloop.SigAlarm ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "request %s timed out" (request_to_string request);
            let output =
              HtmlTemplates.page ~title:"Time Out"
                ~body:(sprintf "Error handling request %s: Timed out after %d seconds"
                         (request_to_string_short request) !Settings.wserver_timeout)
            in
            send_result cout ~error_code:408 output

        | Sys.Break as e ->
            plerror 1 "Break occured while processing HKP request %s"
              (request_to_string request);
            raise e

        | Not_implemented s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Error handling request %s: %s"
              (request_to_string request) ("Not implemented: " ^ s);
            let output =
              HtmlTemplates.page ~title:"Not implemented"
                ~body:(sprintf "Error handling request %s: %s not implemented."
                         (request_to_string request) (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:501 output

        | Page_not_found s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Page not found: %s" s;
            let output = HtmlTemplates.page ~title:"Page not found"
                 ~body:(sprintf "Page not found: %s" (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:404 output

        | Bad_request s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Bad request %s: %s"
              (request_to_string_logdepend request) s;
            let output = HtmlTemplates.page ~title:"Bad request"
                 ~body:(sprintf "Bad request: %s" (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:400 output

        | No_results s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "No results for request %s: %s"
              (request_to_string_logdepend request) s;
            let output = HtmlTemplates.page ~title:"No results found"
             ~body:(sprintf "No results found: %s" (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:404 output

        | Entity_too_large s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Error handling request %s: %s"
              (request_to_string request) s;
            let output = HtmlTemplates.page ~title:"Request Entity Too Large"
                 ~body:(sprintf "Request Entity Too Large: %s" (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:413 output

        | Misc_error s ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Error handling request %s: %s"
              (request_to_string request) s;
            let output = HtmlTemplates.page ~title:"Error handling request"
                 ~body:(sprintf "Error handling request: %s" (HtmlTemplates.html_quote s))
            in
            send_result cout ~error_code:500 output

        | e ->
            ignore (Unix.alarm recover_timeout);
            plerror 2 "Error handling request %s: %s"
              (request_to_string request) (Common.err_to_string e);
            let output =
              (HtmlTemplates.page ~title:"Error handling request"
                 ~body:(sprintf "Error handling request. Exception raised."))
            in
            send_result cout ~error_code:500 output
    with
      | Sys.Break as e -> raise e
      | Eventloop.SigAlarm ->
          ignore (Unix.alarm recover_timeout);
          let output =
            HtmlTemplates.page ~title:"Timeout"
              ~body:(sprintf "Request timed during request parsing after %d seconds"
                       !Settings.wserver_timeout)
          in
          send_result cout ~error_code:408 output
      | e ->
          eplerror 5 e "Miscellaneous error"
  end;
  []
