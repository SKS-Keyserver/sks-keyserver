(***********************************************************************)
(* unit_tests.ml - perform simple unit tests                           *)
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
open Common

let run () =
  printf "Running Decode unit tests:%!";
  begin
    try Decode_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

  printf "Running Number unit tests:%!";
  begin
    try Number_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

  printf "Running Poly unit tests:%!";
  begin
    try Poly_test.run ()
    with Unit_test_failure s ->
      printf "\nUnit test failure: %s\n%!" s
  end;
  printf "Done\n%!";

