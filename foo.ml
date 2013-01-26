(***********************************************************************)
(* foo.ml                                                              *)
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
open Printf
open ZZp
open Number.Infix


let rec gcd_ex' a b =
  if b =! zero then (one,zero,a)
  else
    let (q,r) = quomod_big_int a b in
    let (u',v',gcd) = gcd_ex' b r in
    (v',u' -! v' *! q, gcd)

let gcd_ex a b =
  if b <=! a then gcd_ex' a b
  else
    let (u,v,gcd) = gcd_ex' b a in
    (v,u,gcd)

let gcd_ex_test a b =
     let (a,b) = (big_int_of_int a,big_int_of_int b) in
     let (u,v,gcd) = gcd_ex a b in
     if (u *! a +! v *! b <>! gcd)
     then failwith (sprintf "gcd_ex failed on %s and %s"
                      (string_of_big_int a) (string_of_big_int b))


let run_test () =
  begin
    gcd_ex_test 95 25;
    gcd_ex_test 25 95;
    gcd_ex_test 1 95;
    gcd_ex_test 95 1;
    gcd_ex_test 22 21;
    gcd_ex_test 21 22;
    gcd_ex_test 12 6;
    gcd_ex_test 6 12;
    gcd_ex_test 6 12;
  end
