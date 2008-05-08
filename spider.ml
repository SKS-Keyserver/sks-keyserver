open StdLabels
open MoreLabels
open Printf
open Pstyle
open Common
module Set = PSet.Set
module Unix = UnixLabels

let stats_timeout = 10

let root = ("stinkfoot.org",11370)

let input_lines cin = 
  let rec loop lines = 
    match (try Some (input_line cin)
	   with End_of_file -> None)
    with
      None -> List.rev lines
    | Some l -> loop (l::lines)
  in
  loop []

let get_ip_opt hostname = 
  if hostname = "localhost" then None
  else
    try 
      let he = Unix.gethostbyname hostname in
      Some he.Unix.h_addr_list
    with
      Invalid_argument _ | Not_found -> None

let fetch_url url =
  let cin = Unix.open_process_in (sprintf "curl -s -m %d \"%s\"" stats_timeout url) in
  let lines = input_lines cin in
  match Unix.close_process_in cin with
  | Unix.WEXITED 0 -> Some lines
  | _ -> None

let start_line = Str.regexp ".*Gossip Peers.*"
let whitespace = Str.regexp "[ \t]+"

let get_peer line = 
  if line </> (0,8) = "<tr><td>" then
    match Str.split whitespace (line </> (8,0)) with
    | host::port::_ ->
	let port = int_of_string port in
	Some (host,port)
    | _ -> None
  else
    None

let build_url (host,port) =
  sprintf "http://%s:%d/pks/lookup?op=stats" host port

let lines_to_peers lines = 
  let rec skip_to_start = function
    | line::((_::rest) as tl) -> 
	if Str.string_match start_line line 0 then
	  rest
	else skip_to_start tl
    | _ -> []
  in
  let lines = skip_to_start lines in
  let rec get_peers = function
    | [] -> []
    | hd::tl -> match get_peer hd with
      | Some peer -> peer :: get_peers tl
      | None -> []
  in
  get_peers lines

let multi_fetch (host,port) = 
  let ports = [port+1] in
  (*let ports = if port <> 11370 then 11371::ports else ports in
  let ports = List.rev (80::ports) in *)
  let get_peers (host,port) = 
    match fetch_url (build_url (host,port)) with
    | None -> None
    | Some x -> 
	let peers = lines_to_peers x in
	if peers = [] then None
	else Some peers
  in
  let rec loop ports = match ports with
      [] -> None
    | port::tl -> 
	match get_peers (host,port) with
	| Some x -> Some x
	| None -> loop tl
  in
  loop ports
	
let find_all peer = 
  let visited = ref (Set.singleton None) in
  let rec dfs peer = 
    let ip = get_ip_opt (fst peer) in
    if Set.mem ip !visited then
      []
    else
      begin
	visited := Set.add ip !visited;
	match multi_fetch peer with
	| None -> (* retrieval failed *) 
	    eprintf "(%s,%d) FAILED\n%!" (fst peer) (snd peer);
	    []
	| Some peers -> 
	    try
	      eprintf "(%s,%d)\n%!" (fst peer) (snd peer);
	      let others = List.concat (List.map ~f:dfs peers) in
	      peer :: others
	    with e -> 
	      eprintf "(%s,%d) FAILED with %s\n%!" (fst peer) (snd peer) 
		(Printexc.to_string e);
	      []
      end
  in
  dfs peer


let () = if not !Sys.interactive then 
  let servers = find_all root in
  printf "%d servers found\n" (List.length servers);
  List.iter ~f:(fun (host,port) -> printf "%s %d\n" host port) servers
