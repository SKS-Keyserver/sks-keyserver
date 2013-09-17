(***********************************************************************)
(* mailsync.ml - Code for reading in and processing files received     *)
(*               from PKS-style email-based sync                       *)
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

open Common
open StdLabels
open MoreLabels
open Printf


let max_filesize = 200 * 1024
let input_msg f =
  let b = Buffer.create (min max_filesize (in_channel_length f)) in
  Buffer.add_channel b f (in_channel_length f);
  Buffer.contents b


let dirname = "messages"

let lsdir dir =
  let dirhandle = Unix.opendir dir in
  let run () =
    let rec loop accum =
      match (try Some (Unix.readdir dirhandle)
             with End_of_file -> None)
      with
          Some fname -> loop (fname::accum)
        | None -> accum
    in
    List.map ~f:(Filename.concat dir) (loop [])
  in
  protect ~f:run ~finally:(fun () -> Unix.closedir dirhandle)

(** reads specified mail file and returns key if any *)
let load_message fname =
  let file = open_in fname in
  let run () =
    let text = input_msg file in
    (*let msg = Recvmail.parse text in
      msg.Sendmail.body *)
    text
  in
  protect ~f:run ~finally:(fun () -> close_in file)


let get_mtime fname = (Unix.stat fname).Unix.st_mtime

let demote fname =
  if Sys.file_exists fname then
    let destdir = Lazy.force Settings.failed_msgdir in
    if not (Sys.file_exists destdir) then
      Unix.mkdir destdir 0o700;
    Sys.rename fname (Filename.concat destdir (Filename.basename fname))

(****************************************************************************)
(* Event Handlers  **********************************************************)
(****************************************************************************)

(** read any mails in queue directory, process them, and remove them *)
let rec load_mailed_keys ~addkey () =
  if !Settings.send_mailsyncs then
  (
  plerror 7 "checking for key emails";
  let files = try lsdir (Lazy.force Settings.msgdir) with Unix.Unix_error _ -> [] in
  let ready_files =
    List.filter ~f:(fun file -> Filename.check_suffix file ".ready") files
  in
  List.iter ready_files
    ~f:(fun fname ->
       try
            let text = load_message fname in
            let keys = Armor.decode_pubkey text in
            plerror 3 "Adding list of %d keys from file %s"
              (List.length keys) fname;
            List.iter
              ~f:(fun origkey ->
                    try
                      let key = Fixkey.canonicalize origkey in
                      addkey key
                    with
                        Bdb.Key_exists -> ()
                      | Fixkey.Bad_key ->
                          plerror 2 "Fixkey.canonicalize couldn't parse key %s"
                            (KeyHash.hexify (KeyHash.hash origkey))
                 )
              keys;
            Sys.remove fname
          with
            | Eventloop.SigAlarm | Sys.Break as e -> raise e
            | e ->
                eplerror 2 e "Failure adding keys from file %s. %s"
                  fname "Moving to failed_messages.";
                demote fname
       );
  []
  )
  else
  []

