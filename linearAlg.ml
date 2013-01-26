(***********************************************************************)
(* linearAlg.ml                                                        *)
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
open Printf
open ZZp.Infix

exception Bug of string
exception LayoutMismatch

let rec riter ~f low high =
  if low >= high then ()
  else (
    f low;
    riter ~f (low + 1) high
  )

let rec rfind ~f low high =
  if low >= high then raise Not_found
  else if f(low) then low
  else rfind ~f (low + 1) high



(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

module MatrixSlow =
struct

  type t = { columns: int;
             rows: int;
             array: ZZp.zz array;
           }

  let columns m = m.columns
  let rows m = m.rows
  let dims t = (t.columns,t.rows)

  let copy m = { m with array = Array.copy m.array; }

  let make ~columns ~rows init =
    let array = Array.create (columns * rows) init in
    { columns = columns;
      rows = rows;
      array = array;
    }

  let init ~columns ~rows ~f =
    { columns = columns;
      rows = rows;
      array =
        Array.init (columns * rows)
          ~f:(fun i ->
                let (i,j) = i mod columns, i / columns in
                f i j)
    }

  let get m i j =
    m.array.(i + j * m.columns)

  let set m i j v =
    m.array.(i + j * m.columns) <- v

  let scmult_ip m sc =
    for i = 0 to Array.length m.array - 1 do
      m.array.(i) <- ZZp.mult m.array.(i) sc
    done

  let scmult m v =
    { m with
        array = Array.map ~f:(fun x -> ZZp.mult v x) m.array
    }

  let scmult_row m j sc =
    let start = j * m.columns in
    for i = 0 to m.columns - 1 do
      m.array.(start + i) <- ZZp.mult m.array.(start + i) sc
    done

  let swap_rows m j1 j2 =
    let start1 = j1 * m.columns
    and start2 = j2 * m.columns in
    riter 0 m.columns
      ~f:(fun i ->
            let tmp = m.array.(start1 + i) in
            m.array.(start1 + i) <- m.array.(start2 + i);
            m.array.(start2 + i) <- tmp)

  let add_ip m1 m2 =
    if m1.columns <> m2.columns || m1.rows <> m2.rows then
      raise LayoutMismatch;
    for i = 0 to Array.length m1.array - 1 do
      m1.array.(i) <- ZZp.add m1.array.(i) m2.array.(i)
    done

  let add m1 m2 =
    if m1.columns <> m2.columns || m1.rows <> m2.rows then
      raise LayoutMismatch;
    { m1 with
        array = Array.init (m1.columns * m1.rows)
                  ~f:(fun i -> ZZp.add m1.array.(i) m2.array.(i))
    }

  let rec idot_rec m1 m2 ~i ~pos1 ~pos2 sum =
    if i >= m1.columns then sum
    else
      idot_rec m1 m2 ~i:(i+1) ~pos1:(pos1 + 1) ~pos2:(pos2 + m2.columns)
        (ZZp.add sum (ZZp.mult m1.array.(pos1) m2.array.(pos2)))

  let idot m1 m2 i j =
    idot_rec m1 m2 ~i:0 ~pos1:(m1.columns * i) ~pos2:j ZZp.zero

  let mult m1 m2  =
    if m1.columns <> m2.rows then
      raise LayoutMismatch;
    init ~columns:m2.columns ~rows:m1.rows
      ~f:(fun i j -> idot m1 m2 i j)


  let transpose m =
    init ~columns:m.rows ~rows:m.columns ~f:(fun i j -> get m j i)


  let rowadd m ~src ~dst ~scmult =
    for i = 0 to m.columns - 1 do
      let newval = ZZp.add (ZZp.mult (get m i src) scmult) (get m i dst) in
      set m i dst newval
    done

  let rowsub m ~src ~dst ~scmult =
    if scmult <>: ZZp.one then
      for i = 0 to m.columns - 1 do
        let sval = get m i src in
        if sval <>: ZZp.zero then
          let newval = ZZp.sub (get m i dst) (ZZp.mult_fast sval scmult) in
          set m i dst newval
      done
    else
      for i = 0 to m.columns - 1 do
        let sval = get m i src in
        if sval <>: ZZp.zero then
          let newval = ZZp.sub (get m i dst) sval in
          set m i dst newval
      done

  let print m =
    for j = 0 to m.rows - 1 do
      print_string "| ";
      for i = 0 to m.columns - 1 do
        ZZp.print (get m i j);
        print_string " "
      done;
      print_string " |\n"
    done

end

(*********************************************************************************)
(*********************************************************************************)
(*********************************************************************************)

(* Does everything in-place, using the in-place numerix operators *)
module Matrix =
struct

  type t = { columns: int;
             rows: int;
             array: ZZp.zzref array;
           }

  let columns m = m.columns
  let rows m = m.rows
  let dims t = (t.columns,t.rows)

  let copy m = { m with array = Array.copy m.array; }

  let init ~columns ~rows ~f =
    { columns = columns;
      rows = rows;
      array =
        Array.init (columns * rows)
          ~f:(fun i ->
                let (i,j) = i mod columns, i / columns in
                ZZp.make_ref (f i j))
    }

  let make ~columns ~rows x =
    init ~columns ~rows ~f:(fun i j -> x)

  let lget m i j =
    ZZp.look (m.array.(i + j * m.columns))

  let rget m i j =
    m.array.(i + j * m.columns)

  let get m i j = ZZp.copy_out m.array.(i + j * m.columns)

  let set m i j v =
    ZZp.copy_in m.array.(i + j * m.columns) v

  let scmult_row ?(scol=0) m j sc =
    let start = j * m.columns in
    for i = scol to m.columns - 1 do
      let v = m.array.(start + i) in
      ZZp.mult_in v (ZZp.look v) sc
    done

  let swap_rows m j1 j2 =
    let start1 = j1 * m.columns
    and start2 = j2 * m.columns in
    riter 0 m.columns
      ~f:(fun i ->
            let tmp = ZZp.copy_out m.array.(start1 + i) in
            ZZp.copy_in m.array.(start1 + i) (ZZp.look m.array.(start2 + i));
            ZZp.copy_in m.array.(start2 + i) tmp)

  let transpose m =
    init ~columns:m.rows ~rows:m.columns ~f:(fun i j -> lget m j i)

  let rowsub ?(scol=0) m ~src ~dst ~scmult =
    if scmult <>: ZZp.one then
      for i = scol to m.columns - 1 do
        let sval = rget m i src in
        if ZZp.look sval <>: ZZp.zero then
          let v = rget m i dst in
          ZZp.sub_in v (ZZp.look v) (ZZp.mult_fast (ZZp.look sval) scmult)
      done
    else
      for i = scol to m.columns - 1 do
        let sval = rget m i src in
        if ZZp.look sval <>: ZZp.zero then
          let v = rget m i dst in
          ZZp.sub_in v (ZZp.look v) (ZZp.look sval)
      done

  let print m =
    for j = 0 to m.rows - 1 do
      print_string "| ";
      for i = 0 to m.columns - 1 do
        ZZp.print (lget m i j);
        print_string " "
      done;
      print_string " |\n"
    done

end


(*********************************************************************************)
(*********************************************************************************)
(*********************************************************************************)

(****** Gauss-Jordan Reduction *****************)

let process_row m j =
  try
    let v =
      let v = Matrix.rget m j j in
      if ZZp.look v <>: ZZp.zero then v
      else
        let jswap =
          try
            rfind (j + 1) (Matrix.rows m)
              ~f:(fun jswap -> Matrix.lget m j jswap <>: ZZp.zero)
          with Not_found -> raise Exit
        in
        Matrix.swap_rows m j jswap;
        Matrix.rget m j j
    in
    if ZZp.look v <>: ZZp.one then Matrix.scmult_row m j (ZZp.inv (ZZp.look v));
    for j2 = 0 to Matrix.rows m - 1 do
      if j2 <> j
      then Matrix.rowsub m ~src:j ~dst:j2 ~scmult:(Matrix.get m j j2)
    done
  with
      Exit -> ()

let reduce m =
  let (columns,rows) = Matrix.dims m in
  if columns  < rows then raise (Bug "Matrix is too narrow to reduce");
  for j = 0 to Matrix.rows m - 1 do
    process_row m j;
  done


(****** Gaussian Reduction *****************)

let process_row_forward m j =
  try
    let v =
      let v = Matrix.rget m j j in
      if ZZp.look v <>: ZZp.zero then v
      else
        let jswap =
          try
            rfind (j + 1) (Matrix.rows m)
              ~f:(fun jswap -> Matrix.lget m j jswap <>: ZZp.zero)
          with Not_found -> raise Exit
        in
        Matrix.swap_rows m j jswap;
        Matrix.rget m j j
    in
    if ZZp.look v <>: ZZp.one then Matrix.scmult_row ~scol:j m j (ZZp.inv (ZZp.look v));
    for j2 = j + 1 to Matrix.rows m - 1 do
      Matrix.rowsub ~scol:j m ~src:j ~dst:j2 ~scmult:(Matrix.get m j j2)
    done
  with
      Exit -> ()

let backsubstitute m j =
  if Matrix.lget m j j =: ZZp.one
  then (
    let last = Matrix.rows m - 1 in
    for j2 = j - 1 downto 0 do
      Matrix.rowsub ~scol:last m ~src:j ~dst:j2 ~scmult:(Matrix.get m j j2);
      Matrix.set m j j2 ZZp.zero
    done
  )

let greduce m =
  let (columns,rows) = Matrix.dims m in
  if columns  < rows then raise (Bug "Matrix is too narrow to reduce");
  for j = 0 to Matrix.rows m - 1 do
    process_row_forward m j;
  done;
  for j = Matrix.rows m - 1 downto 1 do
    backsubstitute m j;
  done


let reduce = greduce
