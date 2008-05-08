(************************************************************************)
(* This file is part of SKS.  SKS is free software; you can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA *)
(***********************************************************************)

open StdLabels
open MoreLabels
open Printf
open Scanf
open Common
(** Executable: Uber-executable replacing all others *)

type command = 
    { name: string;
      usage: string;
      desc: string;
      func: unit -> unit
    }

let usage command = 
  sprintf "Usage: sks %s %s" command.name command.usage

let space = Str.regexp " ";;

let rec commands = [
  { name = "db";
    usage = "";
    desc = "Initiates database server";
    func = (fun () -> 
	      let module M = Dbserver.F(struct end) in
	      M.run ()
	   )
  };
  { name = "recon";
    usage = "";
    desc = "Initiates reconciliation server";
    func = (fun () -> 
	      let module M = Reconserver.F(struct end) in
	      M.run ()
	   )
  };
  { name = "cleandb";
    usage = "";
    desc = "Apply filters to all keys in database, fixing some common problems";
    func = (fun () -> 
	      let module M = Clean_keydb.F(struct end) in
	      M.run ()
	   )
  };
  { name = "build";
    usage = "";
    desc = "Build key database, including body of keys directly in database";
    func = (fun () -> 
	      let module M = Build.F(struct end) in
	      M.run ()
	   )
  };
  { name = "fastbuild";
    usage = "-n [size] -cache [mbytes]";
    desc = "Build key database, doesn't include keys directly in database, " ^
	   "faster than build.  -n specifies the mulitple of 15000 keys to " ^
	   "read in at once, and -cache specifies the database cache to use " ^
	   "in megabytes";
    func = (fun () -> 
	      let module M = Fastbuild.F(struct end) in
	      M.run ()
	   )
  };
  { name = "pbuild";
    usage = "-cache [mbytes] -ptree_cache [mbytes]";
    desc = "Build prefix-tree database, used by reconciliation server, " ^
	   "from key database.  Allows for specification of cache for " ^
	   "key database and for ptree database.";
    func = (fun () -> 
	      let module M = Pbuild.F(struct end) in
	      M.run ()
	   )
  };
  { name = "dump";
    usage = "#keys dumpdir";
    desc = "Create a raw dump of the keys in the database";
    func = (fun () -> 
	      let module M = Sksdump.F(struct end) in
	      M.run ()
	   )
  };
  { name = "merge";
    usage = "";
    desc = "Adds key from key files to existing database";
    func = (fun () -> 
	      let module M = Merge_keyfiles.F(struct end) in
	      M.run () 
	   )
  };
  { name = "drop";
    usage = "";
    desc = "Drops key from database.  Requires running sks db.";
    func = Sks_do.drop;
  };
  { name = "update_subkeys";
    usage = "[-n # of updates / 1000]";
    desc = "Updates subkey keyid index to include all current keys.  " ^
	   "Only useful when upgrading versions 1.0.4 or before of sks.";
    func = Update_subkeys.run;
  };
  { name = "incdump";
    usage = "timestamp(seconds since 1970) [dumpname]";
    desc = "Create a raw dump of the keys in the database that got" ^
           "updated after timestamp";
    func = Incdump.run;
  };
  { name = "help";
    usage = "";
    desc = "Prints this message";
    func = help;
  };
]

and help () = 
  printf "This is a list of the available commands\n\n";
  List.iter commands 
    ~f:(fun c ->
	  Format.open_box 3;
	  Format.print_string "sks ";
	  Format.print_string c.name;
	  if c.usage <> "" then (
	    Format.print_string " ";
	    Format.print_string c.usage);
	  Format.print_string ":  ";
	  List.iter (fun s -> 
		       Format.print_string s; 
		       Format.print_space ();)
	    (Str.split space c.desc);
	  Format.close_box ();
	  Format.print_newline ();
       );
printf "\n"
	  

(****************************************************)

let rec find name commands = match commands with
  | [] -> raise Not_found
  | hd::tl -> 
      if hd.name = name 
      then hd else find name tl


let () = 
  match !Settings.anonlist with
    | [] -> 
	eprintf "No command specified\n";
	exit (-1)
    | name::tl -> 
	let command = 
	  try find name commands 
   	  with Not_found ->
	    eprintf "Unknown command %s\n" name;
	    exit (-1)
	in
	Settings.anonlist := tl;
	try command.func ()
	with
	    Argument_error s ->
	      eprintf "Argument error: %s\n" s;
	      eprintf "Usage: sks %s %s\n%!" command.name command.usage;
	      exit (-1)

 	

