/***********************************************************************)
(* crc.c                                                               *)
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
(***********************************************************************/

#include <stdlib.h>
#include <time.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#define CRC24_INIT 0xb704ceL
#define CRC24_POLY 0x1864cfbL

typedef long crc24;
crc24 crc_octets(unsigned char *octets, size_t len) {
  crc24 crc = CRC24_INIT;
  int i;

  while (len--) {
    crc ^= (*octets++) << 16;
    for (i = 0; i < 8; i++) {
      crc <<= 1;
      if (crc & 0x1000000)
        crc ^= CRC24_POLY;
    }
  }
  return crc & 0xffffffL;
}

value caml_crc_octets(value data) {
  CAMLparam1(data);
  CAMLlocal1(rval);
  unsigned char *octets = String_val(data);
  size_t len = string_length(data);
  long crc = crc_octets(octets,len);

  rval = Val_int(crc);
  CAMLreturn(rval);
}


value caml_get_tzname(value none) {
  CAMLparam1(none);
  CAMLlocal1(rval);
  tzset();
  rval = alloc_tuple(2);
  Store_field(rval,0,copy_string(tzname[0]));
  Store_field(rval,1,copy_string(tzname[1]));
  CAMLreturn(rval);
}
