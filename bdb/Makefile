#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.6 2003/07/05 15:16:29 yminsky Exp $
include ../Makefile.local

CINCLUDES=-I`ocamlc -where` $(BDBINCLUDE)
CC=gcc
CXX=g++
CFLAGS=-O3 -Werror-implicit-function-declaration $(CINCLUDES) $(BDBLIB) -I .
CXXFLAGS=-O3 $(CINCLUDES) $(BDBLIB) -I .

MKLIB=ocamlmklib
RANLIB=ranlib
OCAMLDEP=ocamldep $(PP)
CAMLINCLUDE=
COMMONCAMLFLAGS= $(CAMLINCLUDE) $(PP) #-thread
CAMLLIBS=unix.cma str.cma mylibs.cma
OCAMLFLAGS=$(COMMONCAMLFLAGS) -g
OCAMLOPTFLAGS=$(COMMONCAMLFLAGS) -inline 40

ifndef LIBDB
LIBDB=-ldb-4.6
endif

COBJS = bdb_stubs.o

ocextr: ocextr.ml
	$(OCAMLOPT) -o ocextr ocextr.ml

libbdb.a: $(COBJS)
	$(MKLIB) -custom -o bdb $(COBJS)

bdb_stubs.o: bdb_stubs.h bdb_stubs.c

bdb.ml: ocextr bdb_stubs.c
	./ocextr bdb_stubs.c > bdb.ml

bdb.cma: bdb.cmo libbdb.a
	$(MKLIB) -custom -o bdb bdb.cmo -lbdb $(LIBDB)

bdb.cmxa: bdb.cmx libbdb.a
	$(MKLIB) -custom -o bdb bdb.cmx -lbdb $(LIBDB)

bdbcaml: bdb.cma
	ocamlmktop -o bdbcaml -custom unix.cma bdb.cma $^

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o
	rm -f bdb.ml
	rm -f ocextr

install:
	cp libmldb.a $(LIBDIR)/libmldb.a
	cd $(LIBDIR); $(RANLIB) libmldb.a
	cp db.cma db.cmi bdb.mli db.mli $(LIBDIR)

installopt:
	cp db.cmx db.cmxa db.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) db.a



# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.o:
	$(OCAMLOPT) -output-obj $(OCAMLOPTFLAGS) $<

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

.c.obj:
	$(CC) $(CFLAGS) /c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Dependencies
#dep:
#	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

#include .depend


# DO NOT DELETE
