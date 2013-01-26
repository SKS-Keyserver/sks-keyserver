(***********************************************************************)
(* prefixTree.ml                                                       *)
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
module Unix=UnixLabels
(*module ZZp = RMisc.ZZp *)
module Set = PSet.Set
module ZSet = ZZp.Set

exception Bug of string

(** Invariants:
   - Parent of dirty node is dirty.
   - A dirty non-leaf node has at least one dirty child
   - dirty nodes are reachable from the root
   - All nodes not InMem are mirrored on disk.
   - All nodes on disk are in real tree.
*)

(** TODO:
   - Make sure that newly created nodes (in particular, in a split)
     start out Dirty
   - Nodes that are destroyed should have their backing store on disk
     destroyed as well.  In particular, in a join.
*)


type key = Bitstring.t

module WHash =
  Weak.Make(struct
              type t = key
              let equal = (=)
              and hash = Hashtbl.hash
            end)

type writestatus = Clean | Dirty
type 'a disk = OnDisk of key | InMem of 'a

type children = | Leaf of string Set.t
                | Children of node disk array

and node = { svalues: ZZp.mut_array;
             key: key;
             mutable num_elements: int;
             mutable children: children;
             mutable wstatus: writestatus;
           }


type 'txn db = { load : string -> string;
                 save : 'txn option -> key:string -> data:string -> unit;
                 delete : 'txn option -> string -> unit;
                 create_txn : unit -> 'txn option;
                 commit_txn : 'txn option -> unit;
                 abort_txn : 'txn option -> unit;
                 mutable maxnodes : int;
                 mutable inmem_count : int;
               }

type 'txn tree = { root: node;
                   num_samples: int;
                   split_thresh: int; (* threshold for splitting node *)
                   join_thresh: int;   (* threshold for deleting node.
                                          Should be less than split_thresh *)
                   bitquantum: int;    (* amount by which depths differ
                                          from each other *)
                   points: ZZp.zz array;
                   db: 'txn db option;
                   mutable synctime: float;
                 }

type dheader = { d_num_samples: int;
                 d_split_thresh: int;
                 d_join_thresh: int;
                 d_bitquantum: int;
                 d_points: ZZp.zz array;
               }

(******************************************************************)

let op_unwrap x = match x with
    Some y -> y
  | None -> failwith "Attempt to unwrap None"

let op_apply ~f x = match x with
    None -> None
  | Some x -> Some (f x)

let op_map ~f list = List.map ~f:(op_apply ~f) list

(******************************************************************)
(******************************************************************)
(******************************************************************)

(** Returns all extensions of bs to length ~len,
 * starting at bit ~bit
 *)
let rec child_keys_rec bs ~bit ~len =
  if bit >= len
  then
    Set.add (Bitstring.copy bs) Set.empty
  else (
    Bitstring.set bs bit;
    let keys_1 = child_keys_rec bs ~bit:(bit+1) ~len in
    Bitstring.unset bs bit;
    let keys_2 = child_keys_rec bs ~bit:(bit+1) ~len in
    Set.union keys_1 keys_2
  )

(** Return 2^t.bitquantum bitstrings which consist of all possible
  * t.bitquantum-bit extensions of the key.
  *)
let child_keys_raw bitquantum key =
  let len = Bitstring.num_bits key in
  let newlen = len + bitquantum in
  let bs = Bitstring.copy_len key newlen in
  let keys = child_keys_rec bs ~bit:len ~len:newlen in
  Set.elements keys

let child_keys t key = child_keys_raw t.bitquantum key

(******************************************************************)
(******************************************************************)
(******************************************************************)

let marshal_to_string ~f x =
  let bufc = Channel.new_buffer_outc 1000 in
  f (bufc#upcast) x;
  bufc#contents

let unmarshal_of_string ~f s =
  let strc = new Channel.string_in_channel s 0 in
  f (strc#upcast)

(******************************************************************)
(******************************************************************)
(******************************************************************)

let samesize set =
  let sizes = Set.fold ~init:Set.empty set
                ~f:(fun string set -> Set.add (String.length string) set)
  in
  let nsizes = Set.cardinal sizes in
  nsizes = 1 || nsizes = 0

let marshal_node (cout:Channel.out_channel_obj) n =
  cout#write_int n.num_elements;
  cout#write_int (Bitstring.num_bits n.key);
  cout#write_string (Bitstring.to_bytes n.key);
  Array.iter ~f:(fun zz -> cout#write_string (ZZp.to_bytes zz))
    (ZZp.mut_array_to_array n.svalues);
  (match n.children with
       Leaf set ->
         cout#write_byte 1;
         assert (samesize set);
         cout#write_int (Set.cardinal set);
         Set.iter ~f:(fun s -> cout#write_string s) set
     | Children _ ->
         cout#write_byte 0)

let unmarshal_node ~bitquantum ~num_samples (cin:Channel.in_channel_obj) =
  let zzp_len = ZZp.num_bytes () in
  let num_elements = cin#read_int in
  let keybits = cin#read_int in
  let keybytes = Bitstring.bytelength keybits in
  let keydata = cin#read_string keybytes in
  let key = Bitstring.of_bytes keydata keybits in
  let svalues = Array.init num_samples
                  ~f:(fun _ -> ZZp.of_bytes (cin#read_string zzp_len)) in
  let isleaf = cin#read_byte = 1 in
  let children =
    if isleaf then
      let size = cin#read_int in
      let a = Array.init size ~f:( fun i -> cin#read_string zzp_len ) in
      Leaf (Set.of_list (Array.to_list a))
    else
      let ckeys = child_keys_raw bitquantum key in
      Children (Array.map ~f:(fun key -> OnDisk key)
                  (Array.of_list ckeys))
  in
  { svalues = ZZp.mut_array_of_array svalues;
    num_elements = num_elements;
    children = children;
    wstatus = Clean;
    key = key;
  }

let node_to_string n = marshal_to_string ~f:marshal_node n
let node_of_string_raw ~bitquantum ~num_samples s =
  unmarshal_of_string ~f:(unmarshal_node ~bitquantum ~num_samples) s
let node_of_string tree s =
  node_of_string_raw ~bitquantum:tree.bitquantum
    ~num_samples:tree.num_samples s

(******************************************************************)

let marshal_header cout tree =
  ignore (cout :> Channel.out_channel_obj);
  cout#write_int tree.num_samples;
  cout#write_int tree.split_thresh;
  cout#write_int tree.join_thresh;
  cout#write_byte tree.bitquantum;
  Array.iter ~f:(fun zz -> cout#write_string (ZZp.to_bytes zz))
    tree.points

let unmarshal_dheader cin =
  ignore (cin :> Channel.in_channel_obj);
  let zzp_len = ZZp.num_bytes () in
  let num_samples = cin#read_int in
  let split_thresh = cin#read_int in
  let join_thresh = cin#read_int in
  let bitquantum = cin#read_byte in
  let points = Array.init num_samples
                 ~f:(fun zz -> ZZp.of_bytes (cin#read_string zzp_len))
  in
  { d_num_samples = num_samples;
    d_split_thresh = split_thresh;
    d_join_thresh = join_thresh;
    d_bitquantum = bitquantum;
    d_points = points;
  }

(************)

let header_to_string tree =
  marshal_to_string ~f:marshal_header tree

let dheader_of_string s =
  unmarshal_of_string ~f:unmarshal_dheader s

let dheader_to_header db root dh synctime =
  { num_samples = dh.d_num_samples;
    split_thresh = dh.d_split_thresh;
    join_thresh = dh.d_join_thresh;
    bitquantum = dh.d_bitquantum;
    points = dh.d_points;
    db = db;
    root = root;
    synctime = synctime;
  }

(******************************************************************)

let marshal_synctime cout time = cout#write_float time
let unmarshal_synctime cin = cin#read_float

let synctime_to_string time =
  marshal_to_string ~f:marshal_synctime time

let synctime_of_string time =
  unmarshal_of_string ~f:unmarshal_synctime time


(******************************************************************)

(** converts bitstring to dbkey by writing the bitlength of the key followed
  by the bytes of the key itself.

  Note that a more efficient coding is possible, since really you only need 3
  bits, to tell you how much of the last byte is used.
*)
let dbkey_of_key key =
  let bufc = Channel.new_buffer_outc 8 in
  let length = Bitstring.num_bits key in
  let data = Bitstring.to_bytes key in
  bufc#write_int length;
  bufc#write_string data;
  bufc#contents

(** dbkey for storing header *)
let int_to_bstring i =
  let bufc = Channel.new_buffer_outc 1 in
  bufc#write_int i;
  bufc#contents

let root_dbkey = dbkey_of_key (Bitstring.create 0)
let header_dbkey = int_to_bstring (-1)
let synctime_dbkey = int_to_bstring (-2)

(******************************************************************)

(** returns the on-disk version of the node corresponding to dbkey.
  No changes are made to the in-memory tree *)
let load_node tree dbkey =
  let db = op_unwrap tree.db in
  let nodestr = db.load dbkey in
  node_of_string tree nodestr

(** Returns the node corresponding to the [cindex]'th child from the
  [children] array.  If an OnDisk node has been loaded into memory, [children]
  is updated accordingly.
*)
let load_child t children cindex =
  match children.(cindex) with
    | OnDisk key ->
        let db = op_unwrap t.db in
        let cnode = load_node t (dbkey_of_key key) in
        children.(cindex) <- InMem cnode;
        db.inmem_count <- db.inmem_count + 1;
        cnode
    | InMem cnode -> cnode

(** side-effect-free version of load_child *)
let load_child_sef t children cindex =
  match children.(cindex) with
    | OnDisk key -> load_node t (dbkey_of_key key)
    | InMem cnode -> cnode

(******************************************************************)

let save_node t txn node =
  match t.db with
      None -> ()
    | Some db ->
        let dbkey = dbkey_of_key node.key in
        db.save txn ~key:dbkey  ~data:(node_to_string node)

let save_synctime tree txn =
  match tree.db with
      None -> ()
    | Some db ->
        db.save txn ~key:synctime_dbkey
        ~data:(synctime_to_string tree.synctime)


(******************************************************************)
(******************************************************************)
(******************************************************************)

let rec clean_subtree tree txn node = match node.wstatus with
  | Dirty ->
      ( match node.children with
            Leaf _ -> ()
          | Children children ->
              Array.iter children
              ~f:(function
                      OnDisk key -> ()
                    | InMem cnode -> clean_subtree tree txn cnode)
      );
      save_node tree txn node;
      node.wstatus <- Clean;

  | Clean -> ()

let clean txn tree =
  match tree.db with
      None -> ()
    | Some _ ->
        clean_subtree tree txn tree.root;
        save_synctime tree txn


(*************************************************************)

let rec delete_subtree_rec txn tree disknode =
  let node = match disknode with
      InMem node -> node
    | OnDisk key -> load_node tree (dbkey_of_key key)
  in
  let db = op_unwrap tree.db in
  db.delete txn (dbkey_of_key node.key);
  match node.children with
      Leaf _ -> ()
    | Children children ->
        Array.iter ~f:(delete_subtree_rec txn tree) children

let delete_subtree txn tree node =
  perror "Fix this!";
  delete_subtree_rec txn tree (InMem node)

(******************************************************************)
(* Full Tree Summaries  ******************************************)
(******************************************************************)

let rec summarize_tree_rec ~lagg ~cagg tree nodedisk =
  let node = match nodedisk with
      InMem node -> node
    | OnDisk key -> load_node tree (dbkey_of_key key)
  in
  match node.children with
    | Leaf elements ->
        lagg elements
    | Children children ->
        let values =
          Array.map ~f:(summarize_tree_rec ~lagg ~cagg tree) children
        in
        cagg values

let summarize_tree ~lagg ~cagg tree =
  summarize_tree_rec ~lagg ~cagg tree (InMem tree.root)

(******************************************************************)

let depth tree =
  summarize_tree
    ~lagg:(fun _ -> 1)
    ~cagg:(fun depths -> 1 + MArray.max depths)
    tree

let count_nodes tree =
  summarize_tree
    ~lagg:(fun _ -> 1)
    ~cagg:(fun counts -> 1 + Array.fold_left ~f:(+) ~init:0 counts)
    tree

let (<+>) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

(* returns (# internal nodes, # leaf nodes) below & including current node *)
let count_node_types tree =
  summarize_tree
    ~lagg:(fun _ -> (0,1))
    ~cagg:(fun counts ->
             (1,0) <+>
             Array.fold_left ~f:(<+>) ~init:(0,0) counts
          )
    tree

let get_elements tree node =
  summarize_tree_rec
    ~lagg:(fun x -> x)
    ~cagg:(fun sets -> Array.fold_left ~f:Set.union ~init:Set.empty sets)
    tree (InMem node)

let get_zzp_elements tree node =
  let selem = get_elements tree node in
  Set.fold selem ~init:ZSet.empty
    ~f:(fun x set -> ZSet.add (ZZp.of_bytes x) set)

let iter ~f tree =
  summarize_tree
    ~lagg:(Set.iter ~f)
    ~cagg:(fun _ -> ())
    tree

(******************************************************************)

(** returns the number of inmem nodes below and including
  the present node *)
let rec count_inmem node = match node.children with
    Leaf _ -> 1
  | Children children ->
      let counts = Array.map ~f:(function
                                     OnDisk x -> 0
                                   | InMem cnode -> count_inmem cnode)
                     children
      in
      1 + Array.fold_left ~f:(+) ~init:0 counts

(** returns the number of inmen nodes in the tree,
  not counting the root. *)
let count_inmem_tree tree = count_inmem tree.root - 1

let get_inmem_count tree =
  match tree.db with
      None -> raise Not_found
    | Some db -> db.inmem_count

let set_inmem_count tree newcount =
  match tree.db with
      None -> raise Not_found
    | Some db -> db.inmem_count <- newcount



(*************************************************************)
(*  Code for limiting number of InMem nodes  ****************)
(*************************************************************)

let rec list_extract ~f list = match list with
    [] -> []
  | hd::tl -> match f hd with
        None -> list_extract ~f tl
      | Some x -> x::(list_extract ~f tl)

let rec list_prefix k list = match k with
    0 -> []
  | _ -> match list with
        [] -> failwith "Requested prefix longer than list"
      | hd::tl -> hd::(list_prefix (k-1) tl)

let list_prefix_suffix k list =
  let rec loop k list prefix =
    match k with
        0 -> (List.rev prefix,list)
      | _ -> match list with
            [] -> failwith "Requested prefix longer than list"
          | hd::tl ->
              loop (k-1) tl (hd::prefix)
  in
  loop k list []


let inmem_children node = match node.children with
    Leaf _ -> []
  | Children children ->
      list_extract ~f:(function
                           InMem x -> Some x
                         | OnDisk _ -> None
                      )
      (Array.to_list children)

let rec get_frontier tree ~frontier ~newfrontier ~n ~count =
  if count > n then failwith "get_frontier called with count>n"
  else
    match frontier, newfrontier with
      | [],[] ->
          raise (Bug "frontier and newfrontier both empty")
      | [],newfrontier ->
          get_frontier tree ~frontier:newfrontier ~newfrontier:[]
          ~n ~count
      | hd::tl,newfrontier ->
          let children = inmem_children hd in
          let num_kids = List.length children in
          if num_kids + count >= n then
            (List.rev_append frontier newfrontier, count)
          else
            let newfrontier =
              List.rev_append children newfrontier
            in
            let frontier = tl in
            get_frontier tree ~frontier ~newfrontier ~n ~count:(count + num_kids)


(*
let inmem_children node = match node.children with
    Leaf _ -> []
  | Children children ->
      list_extract ~f:(function
                           (i,InMem x) -> Some (i,x)
                         | (i,OnDisk _) -> None )
      (Array.to_list (Array.mapi ~f:(fun i x -> (i,x)) children))

let rec get_frontier tree ~frontier ~newfrontier ~n ~count =
  if count > n then raise (Bug (sprintf "count(%d) exceeded n(%d)" count n))
  else if count = n then (frontier,None)
  else
    match frontier, newfrontier with
        [],[] ->
          raise (Bug "frontier and newfrontier should never both be empty")
      | [],newfrontier ->
          get_frontier tree ~frontier:newfrontier ~newfrontier:[]
          ~n ~count
      | hd::tl, newfrontier ->
          let children = inmem_children hd in
          if List.length children + count <= n then
            let children = List.map ~f:snd children in
            get_frontier tree
              ~frontier:tl
              ~newfrontier:(List.rev_append children newfrontier)
              ~n ~count:(count + List.length children)
          else
            let needed = List.length children + count - n in
            let (needed_children,unneeded_children) =
              list_prefix_suffix needed children in
            (tl @ newfrontier,
             Some (hd,
                   List.map ~f:(fun (i,x) -> x) needed_children,
                   List.map ~f:(fun (i,x) -> i) unneeded_children)
            )
*)


(** marks all the children of a node as being OnDisk *)
let disconnect_children node =
  if node.wstatus = Dirty then
    failwith "Disconnect children called on Dirty node";
  match node.children with
    | Leaf _ -> ()
    | Children children ->
        for i = 0 to Array.length children - 1 do
          match children.(i) with
            | OnDisk key -> ()
            | InMem node -> children.(i) <- OnDisk node.key
        done

(** Reduce number of InMem nodes to no more than n *)
let shrink_tree tree txn n =
  clean txn tree;
  let (frontier,count) = get_frontier tree
                           ~frontier:[ tree.root ]
                           ~newfrontier:[]
                           ~n ~count:0 (* we don't count the root since it's
                                          always in memory *)
  in
  List.iter frontier ~f:disconnect_children;
  let real_count = count_inmem_tree tree  in
  if count <> real_count then
    failwith (sprintf "%s.  expected %d, found %d"
                "tree shrinkage failed to produce tree of expected size"
                count real_count) ;
  set_inmem_count tree count

let shrink_tree_if_necessary tree txn =
  match tree.db with
      None -> ()
    | Some db ->
        if db.inmem_count > db.maxnodes
        then shrink_tree tree txn (db.maxnodes / 2)


(******************************************************************)
(******************************************************************)

let width = 8
let rmask i = 0xFF lsl (width - i)
let lmask i = 0xFF lsr (width - i)

let string_index t depth string =
  let q = t.bitquantum in
  let lowbit = depth * q in
  let highbit = lowbit + q - 1
  in
  let lowbyte = lowbit / 8
  and lowbit = lowbit mod 8
  and highbyte = highbit / 8
  and highbit = highbit mod 8
  in
  if lowbyte = highbyte then
    let byte = int_of_char string.[lowbyte] in
    let key = (byte lsr (7 - highbit)) land
              (lmask (highbit - lowbit + 1)) in
    key
  else  (* extract from two adjacent bytes *)
    let byte1 = int_of_char string.[lowbyte] in
    let byte2 = int_of_char string.[highbyte] in
    let key1 = (byte1 land (lmask (8 - lowbit))) lsl (highbit + 1)  in
    let key2 = (byte2 land (rmask (highbit + 1))) lsr (7 - highbit) in
    let key = key1 lor key2 in
    key

(******************************************************************)

let create_svalues points =
  ZZp.svalues (Array.length points)

let incr_inmem_count tree =
  match tree.db with
      None -> ()
    | Some db ->
        db.inmem_count <- db.inmem_count + 1

let decr_inmem_count tree =
  match tree.db with
      None -> ()
    | Some db ->
        db.inmem_count <- db.inmem_count - 1

let create_node_basic key points =
  { svalues = create_svalues points;
    num_elements = 0;
    children = Leaf Set.empty;
    key = key;
    wstatus = Dirty;
  }

let create_node tree key =
  let points = tree.points in
  incr_inmem_count tree;
  create_node_basic key points

let add_to_node t node zz zzs marray =
  ZZp.mult_array ~svalues:node.svalues marray;
  node.num_elements <- node.num_elements + 1;
  node.wstatus <- Dirty;
  match node.children with
    | Leaf elements ->
        node.children <-
        if Set.mem zzs elements
        then failwith "add_to_node: attempt to reinsert element into prefix tree"
        else Leaf (Set.add zzs elements)
    | _ -> ()

let remove_from_node t node zz zzs marray =
  ZZp.mult_array ~svalues:node.svalues marray;
  node.num_elements <- node.num_elements - 1;
  node.wstatus <- Dirty;
  match node.children with
    | Leaf elements ->
        if not (Set.mem zzs elements)
        then failwith "remove_from_node: attempt to delete non-existant element from prefix tree"
        else node.children <- Leaf (Set.remove zzs elements)
    | _ -> ()


(******************************************************************)

let split_at_depth t zz zzs node depth =
  match node.children with
      Children _ -> raise (Bug "split of non-leaf node.");
    | Leaf elements ->
        let ckeys = Array.of_list (child_keys t node.key) in
        let children =
          Array.map ~f:(fun key -> InMem (create_node t key)) ckeys
        in
        node.children <- Children children;
        Set.iter elements
          ~f:(fun (zzs) ->
                let zz = ZZp.of_bytes zzs in
                let idx = string_index t depth zzs in
                let marray = ZZp.add_el_array ~points:t.points zz in
                let cnode = load_child t children idx in
                add_to_node t cnode zz zzs marray
             )

(******************************************************************)

let pad string bytes =
  let len = String.length string in
  if bytes > len then
    let nstr = String.create bytes in
    String.fill nstr ~pos:len ~len:(bytes - len) '\000';
    String.blit ~src:string ~dst:nstr ~src_pos:0 ~dst_pos:0 ~len;
    nstr
  else
    string



(******************************************************************)
(* Interface functions *******************************************)
(******************************************************************)

let create_empty_header ~points ~bitquantum ~num_samples ~thresh ~dbopt =
  { root = create_node_basic (Bitstring.create 0) points;
    num_samples = num_samples;
    bitquantum = bitquantum;
    split_thresh = thresh;
    join_thresh = thresh / 2;
    points = points;
    db = dbopt;
    synctime = 0.0;
  }

let create ?db:dbopt ~txn ~num_samples ~bitquantum ~thresh () =
  let points = ZZp.points num_samples in
  let dbopt =
    match dbopt with
        None -> None
      | Some (load,save,delete,(create,commit,abort),maxnodes) ->
          Some { load = load;
                 save = save;
                 delete = delete;
                 create_txn = create;
                 commit_txn = commit;
                 abort_txn = abort;
                 maxnodes = maxnodes;
                 inmem_count = 0;
               }
  in
  match dbopt with
      Some db ->
        begin
           try
             let header_string = db.load header_dbkey in
             let dheader = dheader_of_string header_string in

             let root_string = db.load root_dbkey in
             let root = node_of_string_raw ~bitquantum:dheader.d_bitquantum
                 ~num_samples:dheader.d_num_samples root_string in

            let synctime_string = db.load synctime_dbkey in
            let synctime = synctime_of_string synctime_string in

             dheader_to_header dbopt root dheader synctime
           with
               Not_found ->
                (* no header found on disk.  Start from scratch *)
                let tree = create_empty_header ~points ~bitquantum
                             ~num_samples ~thresh ~dbopt in
                (* header and root must now be written to disk *)
                let header_string = header_to_string tree in
                let root_string = node_to_string tree.root in
                let synctime_string = synctime_to_string tree.synctime in
                db.save txn ~key:header_dbkey ~data:header_string;
                db.save txn ~key:root_dbkey ~data:root_string;
                db.save txn ~key:synctime_dbkey ~data:synctime_string;
                tree
        end
    | None ->
        (* No way of accessing the disk, so create a blank tree *)
        create_empty_header ~points ~bitquantum ~num_samples ~thresh ~dbopt

(******************************************************************)

let rec insert_at_depth t zz zzs node marray depth =
  add_to_node t node zz zzs marray;
  (match node.children with
     | Leaf elements ->
         if node.num_elements > t.split_thresh
         then split_at_depth t zz zzs node depth
     | Children children -> (* insertion must continue at next depth *)
         let cindex = string_index t depth zzs in
         let cnode = load_child t children cindex in
         insert_at_depth t zz zzs cnode marray (depth + 1)
  )

let insert_both t txn zz zzs =
  let zzs = pad zzs (ZZp.num_bytes ()) in
  if String.length zzs <> ZZp.num_bytes ()
  then raise (Invalid_argument
                (sprintf "%s.  %d found, %d expected"
                   "PrefixTree.insert_both: zzs has wrong length"
                   (String.length zzs) (ZZp.num_bytes ())
                ));
  let marray = ZZp.add_el_array ~points:t.points zz in
  let root = t.root in
  insert_at_depth t zz zzs root marray 0;
  shrink_tree_if_necessary t txn

let insert t txn zz =
  let zzs = ZZp.to_bytes zz in
  insert_both t txn zz zzs

let insert_str t txn zzs =
  let zz = ZZp.of_bytes zzs in
  insert_both t txn zz zzs

(******************************************************************)

let rec get_ondisk_subkeys tree db key =
  try
    ignore (db.load (dbkey_of_key key));
    let ckeys = child_keys tree key in
    let sets = List.map ~f:(get_ondisk_subkeys tree db) ckeys in
    Set.add key (List.fold_left ~f:Set.union sets ~init:Set.empty)
  with
      Not_found -> (* has no subkeys, so emptyset *)
        Set.empty

let rec delete_at_depth t txn zz zzs node marray depth =
  remove_from_node t node zz zzs marray;
  match node.children with
    | Children children ->
        if node.num_elements <=  t.join_thresh then (
          let elements = Set.remove zzs (get_elements t node) in
          node.children <- Leaf elements;
          match t.db with
              None -> ()
            | Some db ->
                let subkeys = get_ondisk_subkeys t db node.key in
                let subkeys = Set.remove node.key subkeys in
                let inmem_delta = count_inmem node - 1 in
                Set.iter ~f:(fun key -> db.delete txn (dbkey_of_key key))
                  subkeys;
                db.inmem_count <- db.inmem_count - inmem_delta
        ) else (
          let cindex = string_index t depth zzs in
          let cnode = load_child t children cindex in
          delete_at_depth t txn zz zzs cnode marray (depth + 1)
        )
    | _  -> ()

let delete_both t txn zz zzs =
  let zzs = pad zzs (ZZp.num_bytes ()) in
  if String.length zzs <> ZZp.num_bytes ()
  then raise (Invalid_argument
                "PrefixTree.delete_both: zzs has wrong length");
  let marray = ZZp.del_el_array ~points:t.points zz in
  let root = t.root in
  delete_at_depth t txn zz zzs root marray 0


let delete t txn zz =
  let zzs = ZZp.to_bytes zz in
  delete_both t txn zz zzs

let delete_str t txn zzs =
  let zz = ZZp.of_bytes zzs in
  delete_both t txn zz zzs

(******************************************************************)
(******************************************************************)
(******************************************************************)

let set_maxnodes tree txn n =
  match tree.db with
      None -> ()
    | Some db ->
        db.maxnodes <- n;
        shrink_tree_if_necessary tree txn

let get_maxnodes tree =
  match tree.db with
      None -> raise (Invalid_argument
                       "Attempt to invoke DB operation without DB")
    | Some db -> db.maxnodes

(******************************************************************)

let rec get_node_rec ~sef t node zzs ~depth ~goal_depth =
  if depth < goal_depth
  then (
    match node.children with
        Children children ->
          let cindex = string_index t depth zzs in
          let cnode =
            (if sef then load_child_sef else load_child)
                        t children cindex in
          get_node_rec ~sef t cnode zzs ~depth:(depth+1) ~goal_depth
      | Leaf _ ->
          raise Not_found
  )
  else if depth = goal_depth then node
  else failwith "Goal depth exceeded"

let get_node_str ?(sef=false) t zzs depth =
  let rval = get_node_rec ~sef t t.root zzs ~depth:0 ~goal_depth:depth in
  (** shrink the tree if required, creating transaction as needed *)
  begin
    match t.db with
        None -> ()
      | Some db ->
          let txn = db.create_txn () in
          try
            shrink_tree_if_necessary t txn;
            db.commit_txn txn
          with
              e -> db.abort_txn txn; raise e
  end;
  rval


let get_node ?(sef=false) t zz depth =
  let zzs = ZZp.to_bytes zz in
  get_node_str ~sef t zzs depth

let get_node_key ?(sef=false) t key =
  if (Bitstring.num_bits key) mod t.bitquantum <> 0
  then raise (Invalid_argument "Prefix given of wrong length")
  else
    let depth = (Bitstring.num_bits key) / t.bitquantum in
    get_node_str ~sef t (Bitstring.to_bytes key) depth

(******************************************************************)

let root t =  t.root

let children node = match node.children with
  | Leaf _ -> None
  | Children children -> Some children

let svalues node = node.svalues
let size node = node.num_elements
let is_leaf node =
  match node.children with Leaf _ -> true | _ -> false

let points tree = tree.points

let elements tree node =
  let pset = get_elements tree node in
  Set.fold ~f:(fun zzs set -> ZSet.add (ZZp.of_bytes zzs) set)
    ~init:ZSet.empty pset


(******************************************************************)

let node_size tree nodedisk =
  let node = match nodedisk with
      InMem node -> node
    | OnDisk key -> load_node tree (dbkey_of_key key)
  in
  node.num_elements

let nonempty_children tree children =
  let sizes = Array.map ~f:(node_size tree) children in
  let nonempty = Array.mapi ~f:(fun i s -> (i,s > 0) ) sizes in
  Array.fold_left ~f:(fun list (i,nonempty) ->
                        if nonempty then i::list else list)
    ~init:[] nonempty

let random_element list =
  let i = Random.int (List.length list) in
  List.nth list i

let rec get_random tree node =
  match node.children with
      Leaf children ->
        if Set.is_empty children then raise Not_found
        else
          let elements = Set.elements children in
          let i = Random.int (Set.cardinal children) in
          List.nth elements i
    | Children children ->
        let nonempty = nonempty_children tree children in
        if List.length nonempty = 0
        then raise (Bug "Internal node with no nonempty children");
        let randchild =
          match children.(random_element nonempty) with
              InMem node -> node
            | OnDisk key -> load_node tree (dbkey_of_key key)
        in
        get_random tree randchild


let set_synctime tree synctime = tree.synctime <- synctime
let get_synctime tree = tree.synctime

let depth tree node = Bitstring.num_bits node.key / tree.bitquantum
let num_elements tree node = node.num_elements


(******************************************************************)
(******************************************************************)
(******************************************************************)

