(***********************************************************************)
(* bitstring.ml                                                        *)
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

module Unix=UnixLabels

exception Error of string
exception LengthError of string

let width = 8

type t = { a: string;
           bitlength: int;
         }

let bytelength bits =
  bits / width + (if bits mod width = 0 then 0 else 1)

let create bits =
  let bytes = bytelength bits
  in
  { a = String.create bytes;
    bitlength = bits;
  }

let get ba bit =
  let byte_pos = bit / width
  and bit_pos = bit mod width in
  let intval = int_of_char (String.get ba.a byte_pos) in
  (intval lsr (width - bit_pos - 1)) land 1

let lget ba bit = get ba bit = 1

let flip ba bit =
  let byte_pos = bit / width
  and bit_pos = bit mod width in
  let intval = int_of_char (String.get ba.a byte_pos) in
  let new_char = char_of_int ((1 lsl (width - bit_pos - 1)) lxor intval)
  in
  String.set ba.a byte_pos new_char

let set ba bit =
  let byte_pos = bit / width
  and bit_pos = bit mod width in
  let intval = int_of_char (String.get ba.a byte_pos) in
  let new_char = char_of_int ((1 lsl (width - bit_pos - 1)) lor intval)
  in
  String.set ba.a byte_pos new_char

let unset ba bit =
  let byte_pos = bit / width
  and bit_pos = bit mod width in
  let intval = int_of_char (String.get ba.a byte_pos) in
  let new_char = char_of_int ((lnot (1 lsl (width - bit_pos - 1)))
                              land intval)
  in
  String.set ba.a byte_pos new_char

let setval ba bit bool =
  if bool then set ba bit else unset ba bit

(************************************************************)
(* Printing and Conversions *********************************)
(************************************************************)

let print ba =
  for i = 0 to ba.bitlength - 1 do
    if get ba i = 0
    then print_string "0"
    else print_string "1"
  done

let hexprint ba =
  print_string (Utils.hexstring ba.a)

let to_bool_array ba =
  Array.init ~f:(fun i -> lget ba i) ba.bitlength

let to_string ba =
  let string = String.create ba.bitlength in
  for i = 0 to ba.bitlength -1 do
    if get ba i = 0 then string.[i] <- '0' else string.[i] <- '1'
  done;
  string

let to_bytes ba =
  let lastbit = (bytelength ba.bitlength)*width - 1 in
  for i = ba.bitlength to lastbit do
    unset ba i
  done;
  String.sub ~pos:0 ~len:(bytelength ba.bitlength) ba.a

let of_bytes string bitlength =
  { bitlength = bitlength;
    a = String.copy string;
  }

let of_byte b =
  { bitlength = width;
    a = String.make 1 (char_of_int (b land 0xFF));
  }

let of_bytes_all string =
  { bitlength = (String.length string) * width;
    a = String.copy string;
  }

let of_int i =
  { bitlength = width * 4;
    a = Utils.bstring_of_int i;
  }

let of_bytes_nocopy string bitlength =
  { bitlength = bitlength;
    a = string;
  }

let of_bytes_all_nocopy string =
  { bitlength = (String.length string) * width;
    a = string;
  }

let to_bytes_nocopy ba =
  let lastbit = (bytelength ba.bitlength)*8 - 1 in
  for i = ba.bitlength to lastbit do
    unset ba i
  done;
  ba.a

(************************************************************)
(************************************************************)
(************************************************************)

let copy ba = { ba with a = String.copy ba.a }

(** returns a copy of bitstring copied into a new bitstring of a new length.
  No guarantees are made as to the contents of the remainder of the bitstring
  if the bitstring length is extended.
 *)
let copy_len ba bitlength =
  let bytes = bytelength bitlength in
  let str = String.create bytes in
  String.blit ~src:ba.a ~src_pos:0
    ~dst:str ~dst_pos:0 ~len:(String.length ba.a);
  { a = str; bitlength = bitlength }

(********************************************************************)
(***  Shifting  *****************************************************)
(********************************************************************)

let shift_pair_left c1 c2 bits=
  let i1 = int_of_char c1
  and i2 = int_of_char c2  in
  let shifted_int =
    (i1 lsl bits) lor (i2 lsr (width - bits))
  in
  char_of_int (shifted_int land 0xFF)

let shift_pair_right c1 c2 bits =
  let i1 = int_of_char c1
  and i2 = int_of_char c2 in
  let shifted_int =
    (i1 lsl (width - bits)) lor (i2 lsr bits)
  in
  char_of_int (shifted_int land 0xFF)

(**********************************)

let shift_left_small ba bits =
  if bits > 0 then
    let bytes = bytelength ba.bitlength in
    for i = 0 to bytes-2 do
      ba.a.[i] <- shift_pair_left ba.a.[i] ba.a.[i+1] bits
    done;
    ba.a.[bytes-1] <- shift_pair_left ba.a.[bytes-1] '\000' bits

let shift_right_small ba bits =
  if bits > 0 then
    let bytes = bytelength ba.bitlength in
    for i = bytes-1 downto 1 do
      ba.a.[i] <- shift_pair_right ba.a.[i-1] ba.a.[i] bits
    done;
    ba.a.[0] <-  shift_pair_right '\000' ba.a.[0] bits

(**********************************)

let rec shift_left ba bits =
  if bits < 0 then
    shift_right ba (-bits)
  else
  let bytelength = bytelength ba.bitlength
  and bytes = bits / width
  and bits = bits mod width in
  if bytes > 0
  then
    begin
      for i = 0 to bytelength - 1 - bytes do
        ba.a.[i] <- ba.a.[i+bytes];
      done;
      for i = bytelength - bytes to bytelength - 1 do
        ba.a.[i] <- '\000'
      done
    end;
  shift_left_small ba bits

and shift_right ba bits =
  if bits < 0 then
    shift_left ba (-bits)
  else
    let bytelength = bytelength ba.bitlength
    and bytes = bits / width
    and bits = bits mod width in
    if bytes > 0
    then
      begin
        for i = bytelength - 1 downto bytes do
          ba.a.[i] <- ba.a.[i-bytes];
        done;
        for i = bytes - 1 downto 0 do
          ba.a.[i] <- '\000'
        done
      end;
    shift_right_small ba bits

let num_bits ba = ba.bitlength
let num_bytes ba = bytelength ba.bitlength

(********************************************************************)
(********************************************************************)
(********************************************************************)

let rmasks =
  Array.init width ~f:(fun i -> 0xFF lsl (width - i))

(* Later, extend to have optional initial-position arguments *)
let blit ~src ~dst ~len =
  (* these tests are probably redundant, since they'll cause
     exceptions deeper in.  OCaml's lousy traceback features, however, make
     it somewhat useful to have these here. *)
  if len < 0
  then raise (Invalid_argument "Bitstring.blit: negative len");
  if dst.bitlength < len
  then raise (Invalid_argument "Bitstring.blit: dst too short");
  if src.bitlength < len
  then raise (Invalid_argument "Bitstring.blit: src too short");
  let bytelen = len / width
  and bitlen = len mod width in
  String.blit
    ~src:src.a ~src_pos:0
    ~dst:dst.a ~dst_pos:0 ~len:bytelen;
  if bitlen > 0 then
    let srcval = int_of_char (String.get src.a bytelen)
    and dstval = int_of_char (String.get dst.a bytelen) in
    let newdst = (rmasks.(bitlen) land srcval) lor
                 ((lnot rmasks.(bitlen)) land dstval)
    in
    dst.a.[bytelen] <- char_of_int newdst


(* let full_blit ~src ~src_pos ~dst ~dst_pos ~len =  *)


let zero_out bs =
  String.fill bs.a ~pos:0 ~len:(String.length bs.a) '\000'

(*
let extract bs ~pos ~len =
  let first_bit = pos % 8
  let first_byte = pos / 8 in
  let last_byte = (pos + len) / 8 +
                  (if (pos + len) % 8 > 0 then 1 else 0) in
  let byte_len =  last_byte - first_byte + 1 in
  let newbs = Bitstring.create len in
  String.blit
    ~src:bs.a ~src_pos:src_first_byte
    ~dst:newbs.a ~dst_pos:0 ~len:byte_len;
  shift_left newbs first_bit;
*)


(*
let concat bs1 bs2 =
  let newbs = create (bs1.bits + bs2.bits) in
  blit ~src:bs1 ~dst:newbs ~len:(bs1.bits);
*)


