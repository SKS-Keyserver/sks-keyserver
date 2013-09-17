(***********************************************************************)
(* membership.mli - Module for tracking gossip membership and mailsync *)
(*                  peers                                              *)
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

(** Reset the last time the mtime was read to zero, to force the    *)
(** membership file to be reloaded from disk                        *)
val reset_membership_time : unit -> unit

(** Get human-readable names of gossip peers.                       *)
val get_names : unit -> string array

(** Picks single gossip partner from list of possible partners, and *)
(** returns list of all known addresses for that host               *)
val choose : unit -> UnixLabels.addr_info list

(** Returns true iff the address in question belongs to one of the  *)
(** hosts on the gossip membership list.                            *)
val test : UnixLabels.sockaddr -> bool

(** Returns the list of email addresses for use in PKS-style key    *)
(** distribution                                                    *)
val get_mailsync_partners : unit -> string list
