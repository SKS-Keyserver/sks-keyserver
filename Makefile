#
#   This file is part of SKS.  SKS is free software; you can
#   redistribute it and/or modify it under the terms of the GNU General
#   Public License as published by the Free Software Foundation; either
#   version 2 of the License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
#   USA
#
CINCLUDES=-I`ocamlc -where`
CC=gcc
CXX=g++
CFLAGS=-O3 -Werror-implicit-function-declaration $(CINCLUDES) -I .
CXXFLAGS=-O3 $(CINCLUDES) -I .

ifndef OCAMLC
	OCAMLC=ocamlc
endif
ifndef OCAMLOPT
	OCAMLOPT=ocamlopt
endif
ifndef CAMLP4O
	CAMLP4O=camlp4o
endif

export OCAMLC
export OCAMLOPT
export CAMLP4O

include Makefile.local

ifndef PREFIX
	PREFIX=/usr/local
endif
ifeq ($(BDBLIB),)
	OCAMLLIB=
else
	OCAMLLIB= -ccopt $(BDBLIB)
endif

SKSVS=$(shell grep 'version_suffix = "+"' common.ml)
ifeq ($(strip $(SKSVS)),)
WARNERR=
else
WARNERR=-warn-error A
endif

CAMLP4=-pp $(CAMLP4O)
CAMLINCLUDE= -I lib -I bdb
COMMONCAMLFLAGS=$(CAMLINCLUDE) $(OCAMLLIB) -ccopt -Lbdb -dtypes $(WARNERR)
OCAMLDEP=ocamldep $(CAMLP4)
CAMLLIBS=unix.cma str.cma bdb.cma nums.cma bigarray.cma cryptokit.cma
OCAMLFLAGS=$(COMMONCAMLFLAGS) -g $(CAMLLIBS)
OCAMLOPTFLAGS=$(COMMONCAMLFLAGS) -inline 40 $(CAMLLIBS:.cma=.cmxa)

EXE=sks sks_add_mail
ALL=$(EXE) sks.8.gz
ALL.bc=$(EXE:=.bc) sks.8.gz

all: $(ALL)
all.bc: $(ALL.bc)

COBJS=crc.o

MOBJS.bc= pSet.cmo pMap.cmo utils.cmo heap.cmo mList.cmo \
       mTimer.cmo mArray.cmo

MOBJS=$(MOBJS.bc:.cmo=.cmx)

ROBJS.bc= settings.cmo pstyle.cmo getfileopts.cmo \
	common.cmo channel.cmo eventloop.cmo ehandlers.cmo \
	bitstring.cmo meteredChannel.cmo \
	number.cmo prime.cmo zZp.cmo rMisc.cmo \
	linearAlg.cmo poly.cmo decode.cmo \
	fqueue.cmo prefixTree.cmo msgContainer.cmo \
	nbMsgContainer.cmo cMarshal.cmo reconMessages.cmo \
	server.cmo client.cmo reconCS.cmo \
	number_test.cmo decode_test.cmo poly_test.cmo \
	Unique_time.cmo version.cmo
ROBJS=$(ROBJS.bc:.cmo=.cmx)

OBJS.bc=packet.cmo parsePGP.cmo sStream.cmo bdbwrap.cmo \
	key.cmo keyHash.cmo keyMerge.cmo fixkey.cmo \
	fingerprint.cmo keydb.cmo armor.cmo \
	dbMessages.cmo htmlTemplates.cmo wserver.cmo \
	membership.cmo tester.cmo request.cmo \
	stats.cmo index.cmo mRindex.cmo pTreeDB.cmo \
	sendmail.cmo recvmail.cmo mailsync.cmo \
	clean_keydb.cmo build.cmo fastbuild.cmo pbuild.cmo merge_keyfiles.cmo \
	sksdump.cmo incdump.cmo dbserver.cmo reconComm.cmo recoverList.cmo \
	catchup.cmo reconserver.cmo update_subkeys.cmo sks_do.cmo unit_tests.cmo

OBJS=$(OBJS.bc:.cmo=.cmx)

RSERVOBJS.bc=reconComm.cmo recoverList.cmo catchup.cmo reconserver.cmo
RSERVOBJS=$(RSERVOBJS.bc:.cmo=.cmx)

ALLOBJS.bc=$(COBJS) $(MOBJS.bc) $(ROBJS.bc) $(OBJS.bc)
ALLOBJS=$(ALLOBJS.bc:.cmo=.cmx)

EXEOBJS.bc=$(RSERVOBJS.bc) build.cmo fastbuild.cmo dbserver.cmo pdiskTest.cmo

LIBS.bc= lib/cryptokit.cma bdb/bdb.cma
LIBS=$(LIBS.bc:.cma=.cmxa)

VERSION := $(shell cat VERSION)
VERSIONPREFIX = sks-$(VERSION)
COMMA_VERSION := $(shell cat VERSION | sed y/./,/)
FILES := $(shell sed s/.*/$(VERSIONPREFIX)\\/\&/ FILES)

# Special case make rules for functions which require preprocessor directives

common.cmx: common.ml VERSION
	$(OCAMLOPT) $(OCAMLOPTFLAGS) \
	-pp "sed s/__VERSION__/$(COMMA_VERSION)/" -c $<

common.cmo: common.ml VERSION
	$(OCAMLC) $(OCAMLFLAGS) -pp "sed s/__VERSION__/$(COMMA_VERSION)/" -c $<

keyMerge.cmo: keyMerge.ml
	$(OCAMLC) $(OCAMLFLAGS) $(CAMLP4) -c $<

keyMerge.cmx: keyMerge.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(CAMLP4) -c $<

# Special targets

install:
	mkdir -p $(PREFIX)/bin
	install sks_build.sh sks sks_add_mail $(PREFIX)/bin
	mkdir -p $(MANDIR)/man8
	install sks.8.gz $(MANDIR)/man8

install.bc:
	mkdir -p $(PREFIX)/bin
	install sks_build.bc.sh sks.bc sks_add_mail.bc $(PREFIX)/bin
	mkdir -p $(MANDIR)/man8
	install sks.8.gz $(MANDIR)/man8


Makefile.local:
	@if [ ! -e Makefile.local ]; then echo "Makefile.local has to be defined before building. See Makefile.local.unused"; exit 1; fi;

src:
	if [ ! -x $(VERSIONPREFIX) ]; then ln -s . $(VERSIONPREFIX); fi
	tar cfz $(VERSIONPREFIX).tgz $(FILES)
	rm $(VERSIONPREFIX)

# Ordinary targets

sks.8.gz: sks.8
	gzip -f sks.8

sks.8: sks.pod
	pod2man -c "SKS OpenPGP Key server" --section 8 -r 0.1 -name sks sks.pod sks.8

spider: $(LIBS) $(ALLOBJS) spider.cmx
	$(OCAMLOPT) -o spider $(OCAMLOPTFLAGS) $(ALLOBJS) spider.cmx

spider.bc: $(LIBS.bc) $(ALLOBJS.bc) spider.cmo
	$(OCAMLC) -o spider.bc $(OCAMLFLAGS) $(ALLOBJS.bc) spider.cmo

sksclient: $(LIBS) $(ALLOBJS) sksclient.cmx
	$(OCAMLOPT) -o sksclient $(OCAMLOPTFLAGS) $(ALLOBJS) sksclient.cmx

sksclient.bc: $(LIBS.bc) $(ALLOBJS.bc) sksclient.cmo
	$(OCAMLC) -o sksclient.bc $(OCAMLFLAGS) $(ALLOBJS.bc) sksclient.cmo

sks: $(LIBS) $(ALLOBJS) sks.cmx
	$(OCAMLOPT) -o sks $(OCAMLOPTFLAGS) $(ALLOBJS) sks.cmx

sks.bc: $(LIBS.bc) $(ALLOBJS.bc) sks.cmo
	$(OCAMLC) -o sks.bc $(OCAMLFLAGS) $(ALLOBJS.bc) sks.cmo

nbtest.bc: $(LIBS.bc) $(ALLOBJS.bc) nbtest.cmo
	$(OCAMLC) -o nbtest.bc $(OCAMLFLAGS) $(ALLOBJS.bc) nbtest.cmo

ptest: $(LIBS) $(ALLOBJS) ptest.cmx
	$(OCAMLOPT) -o ptest $(OCAMLOPTFLAGS) $(ALLOBJS) \
	ptest.cmx

ptree_consistency_test: $(LIBS) $(ALLOBJS) reconPTreeDb.cmx \
		ptree_consistency_test.cmx
	$(OCAMLOPT) -o ptree_consistency_test $(OCAMLOPTFLAGS) $(ALLOBJS) \
	reconPTreeDb.cmx ptree_consistency_test.cmx

ptree_consistency_test.bc: $(LIBS.bc) $(ALLOBJS.bc) reconPTreeDb.cmo \
		ptree_consistency_test.cmo
	$(OCAMLC) -o ptree_consistency_test.bc $(OCAMLFLAGS) $(ALLOBJS.bc) \
	reconPTreeDb.cmo ptree_consistency_test.cmo

ptree_db_test: $(LIBS) $(ALLOBJS) reconPTreeDb.cmx \
		ptree_db_test.cmx
	$(OCAMLOPT) -o ptree_db_test $(OCAMLOPTFLAGS) $(ALLOBJS) \
	reconPTreeDb.cmx ptree_db_test.cmx

ptree_db_test.bc: $(LIBS.bc) $(ALLOBJS.bc) reconPTreeDb.cmo \
		ptree_db_test.cmo
	$(OCAMLC) -o ptree_db_test.bc $(OCAMLFLAGS) $(ALLOBJS.bc) \
	reconPTreeDb.cmo ptree_db_test.cmo

sks_do.bc: $(LIBS.bc) $(ALLOBJS.bc) sks_do.cmo
	$(OCAMLC) -o sks_do.bc $(OCAMLFLAGS) $(ALLOBJS.bc) sks_do.cmo

sks_do: $(LIBS) $(ALLOBJS) sks_do.cmx
	$(OCAMLOPT) -o sks_do $(OCAMLOPTFLAGS) $(ALLOBJS) sks_do.cmx


sks_add_mail.bc: pMap.cmo pSet.cmo add_mail.cmo
	$(OCAMLC) -o sks_add_mail.bc -g unix.cma \
	pMap.cmo pSet.cmo add_mail.cmo

sks_add_mail: $(LIBS) pMap.cmx pSet.cmx add_mail.cmx
	$(OCAMLOPT) -o sks_add_mail unix.cmxa \
	pMap.cmx pSet.cmx add_mail.cmx

ocamldoc.out: $(ALLOBJS) $(EXEOBJS)
	ocamldoc -hide Pervasives,UnixLabels,MoreLabels \
	-dot $(CAMLP4O) -d doc -I lib -I bdb *.mli *.ml

sks_logdump.bc: $(LIBS.bc) $(ALLOBJS.bc) logdump.cmo
	$(OCAMLC) -o sks_logdump.bc $(OCAMLFLAGS) $(ALLOBJS.bc) logdump.cmo

sks_logdump: $(LIBS) $(ALLOBJS) logdump.cmx
	$(OCAMLOPT) -o sks_logdump $(OCAMLOPTFLAGS) $(ALLOBJS) \
	logdump.cmx

bugscript: $(LIBS) $(ALLOBJS) reconPTreeDb.cmx bugscript.cmx
	$(OCAMLOPT) -o bugscript $(OCAMLOPTFLAGS) $(ALLOBJS) \
	reconPTreeDb.cmx bugscript.cmx

bugscript.bc: $(LIBS.bc) $(ALLOBJS.bc) reconPTreeDb.cmo bugscript.cmo
	$(OCAMLC) -o bugscript.bc $(OCAMLFLAGS) $(ALLOBJS.bc) \
	reconPTreeDb.cmo bugscript.cmo

ptree_replay: $(LIBS) $(ALLOBJS) reconPTreeDb.cmx ptree_replay.cmx
	$(OCAMLOPT) -o ptree_replay $(OCAMLOPTFLAGS) $(ALLOBJS) \
	reconPTreeDb.cmx ptree_replay.cmx

modules.dot: ocamldoc.out
	./recolor.py < ocamldoc.out > modules.dot

modules.ps: modules.dot
	dot -Nfontsize=200 modules.dot -Tps -o modules.ps

doc: $(ALLOBJS) $(EXEOBJS)
	mkdir -p doc
	ocamldoc -hide Pervasives,UnixLabels,MoreLabels \
	-html $(CAMLP4O) -d doc -I lib -I bdb *.mli *.ml

##################################
# LIBS
##################################

bdb/bdb.cmxa: bdb/bdb_stubs.c bdb/bdb_stubs.h
	cd bdb && $(MAKE) bdb.cmxa

bdb/bdb.cma: bdb/bdb_stubs.c bdb/bdb_stubs.h
	cd bdb && $(MAKE) bdb.cma

bdbclean:
	cd bdb && $(MAKE) clean

##################################


prepared:
	mkdir -p lib
	mkdir -p tmp/bin
	mkdir -p tmp/include
	touch prepared


CKVER=cryptokit-1.7
CKDIR=$(CKVER)/src

$(CKVER)/README.txt:
	tar xmvfz $(CKVER).tar.gz
	patch -p 0 < $(CKVER)-sks.patch
	patch -p 0 < $(CKVER)-sks-custom_compare.patch

$(CKDIR)/cryptokit.cma: $(CKVER)/README.txt
	cd $(CKDIR) && $(MAKE) all

$(CKDIR)/cryptokit.cmxa: $(CKVER)/README.txt
	cd $(CKDIR) && $(MAKE) allopt

lib/cryptokit.cma: $(CKDIR)/cryptokit.cma $(CKDIR)/cryptokit.cmxa prepared
	cp $(CKDIR)/cryptokit.cmi $(CKDIR)/cryptokit.cma \
	   $(CKDIR)/cryptokit.mli lib
	cp $(CKDIR)/libcryptokit.a lib
	if test -f $(CKDIR)/dllcryptokit.so; then \
	   cp $(CKDIR)/dllcryptokit.so lib; fi
	if test -f $(CKDIR)/cryptokit.cmxa; then \
	   cp $(CKDIR)/cryptokit.cmxa $(CKDIR)/cryptokit.cmx \
	   $(CKDIR)/cryptokit.a lib; fi

lib/cryptokit.cmxa: lib/cryptokit.cma

################################
# old stuff
################################
prefix_test: $(ALLOBJS) prefix_test.cmx
	$(OCAMLOPT) -o prefix_test $(OCAMLOPTFLAGS) $(ALLOBJS) prefix_test.cmx

prefix_test.opt: $(ROBJS.opt) prefix_test.cmx
	$(OCAMLOPT) -o prefix_test.opt $(OCAMLOPTFLAGS) $(ROBJS.opt) \
	prefix_test.cmx

pdiskTest: $(LIBS) $(MOBJS) $(ROBJS) pdiskTest.cmo
	$(OCAMLC) -o pdiskTest $(OCAMLFLAGS) $(MOBJS) $(ROBJS) pdiskTest.cmo

pdiskTest.opt: $(LIBS.opt) $(MOBJS.opt) $(ROBJS.opt) pdiskTest.cmx
	$(OCAMLOPT) -o pdiskTest.opt $(OCAMLOPTFLAGS) \
	$(MOBJS.opt) $(ROBJS.opt) pdiskTest.cmx

pdtcaml: $(LIBS) $(ROBJS) pdiskTest.cmo
	ocamlmktop -o pdtcaml -custom $(CAMLLIBS) $(CAMLINCLUDE) \
	$(ROBJS) pdiskTest.cmo

script: $(LIBS) $(ALLOBJS) script.cmo
	$(OCAMLC) -o script $(OCAMLFLAGS) $(ALLOBJS) script.cmo

dbtest.bc: $(LIBS.bc) $(ALLOBJS.bc) dbtest.cmo
	$(OCAMLC) -o dbtest.bc $(OCAMLFLAGS) $(ALLOBJS.bc) dbtest.cmo

dbtest: $(LIBS) $(ALLOBJS) dbtest.cmx
	$(OCAMLOPT) -o dbtest $(OCAMLOPTFLAGS) $(ALLOBJS) dbtest.cmx

tester: $(LIBS) $(ALLOBJS) tester.cmo
	$(OCAMLC) -o tester $(OCAMLFLAGS) $(ALLOBJS) tester.cmo

dumbloop: $(LIBS) $(ALLOBJS) dumbloop.cmo
	$(OCAMLC) -o dumbloop $(OCAMLFLAGS) $(ALLOBJS) dumbloop.cmo

scan: $(OBJS) cryptokit dblib scan.ml
	$(OCAMLC) -o scan $(OCAMLFLAGS) $(OBJS) scan.ml

query: $(LIBS) $(ALLOBJS) query.cmo
	$(OCAMLC) -o query $(OCAMLFLAGS) $(ALLOBJS) query.cmo

printids: $(OBJS:.cmo=.cmx) cryptokit printids.ml
	$(OCAMLOPT) -o printids $(OCAMLOPTFLAGS) $(OBJS:.cmo=.cmx) printids.ml

printids.bc: $(OBJS) cryptokit printids.ml
	$(OCAMLC) -o printids $(OCAMLFLAGS) $(OBJS) printids.ml

krecode: $(ALLOBJS.opt) $(LIBS) recode.ml
	$(OCAMLOPT) -o krecode $(OCAMLOPTFLAGS) $(ALLOBJS.opt) recode.ml

rcaml: $(LIBS.bc) $(ALLOBJS.bc)
	ocamlmktop -o rcaml -custom $(CAMLLIBS) $(CAMLINCLUDE) \
	$(ALLOBJS.bc) $(OCAMLLIB)



# Common rules
.SUFFIXES: .mli .ml .cmo .cmi .cmx

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


# Clean up
mlclean:
	rm -f *.cm[iox]
	rm -f *.annot
	rm -f *.opt
	rm -f *.bc
	rm -rf spider sksclient
	rm -f $(ALL) $(ALL.bc)

clean: mlclean
	rm -f *.o
	rm -f prepared
	rm -f sks.8.gz

cleanall: clean bdbclean
	rm -f lib/*
	rm -rf $(CKVER)

distclean: cleanall
	rm -rf Makefile.local
	rm -rf .depend tmp lib

# Dependencies

dep:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend

# DO NOT DELETE
