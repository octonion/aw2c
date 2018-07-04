# Makefile -- build, test and install aw2c

# This file is part of aw2c. Copyright 2008 Glyn Webster.
# 
# aw2c is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# aw2c is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public
# License along with aw2c.  If not, see <http://www.gnu.org/licenses/>.


# Where the aw2c files will be installed. Edit these to suit your system:

PREFIX = /usr/local
BINDIR = ${PREFIX}/bin
INCDIR = ${PREFIX}/include
LIBDIR = ${PREFIX}/lib
DOCDIR = ${PREFIX}/share/doc/aw2c
MANDIR = ${PREFIX}/share/man/man1


# ------------------------------------------------------------------------------

.phony:: default build documentation all install test clean dist version
.phony:: install-program install-manual

default: test

all: build documentation

install: install-program install-manual

install-program: build test aw2c.1
	install -m 755 -d $(BINDIR) $(LIBDIR) $(INCDIR) $(MANDIR)
	install -m 755 -t $(BINDIR) aw2c algolw
	install -m 644 -t $(LIBDIR) libalw.a 
	install -m 644 -t $(INCDIR) alw.h 
	install -m 644 -t $(INCDIR) alwio.h 
	install -m 644 -t $(MANDIR) aw2c.1
	ln -s -f $(MANDIR)/aw2c.1 $(MANDIR)/algolw.1

install-manual: aw2c.html
	install -m 755 -d $(DOCDIR)
	install -m 644 -t $(DOCDIR) aw2c.html $(HTMLMAN)


# ------------------------------------------------------------------------------

build: version.ml libalw.a byte-code

OCAMLMAKEFILE = OCamlMakefile

RESULT = aw2c

PRE_TARGETS += version.ml

version.ml: version
	echo "let release = \"$(shell cat VERSION)\"" > version.ml

ML      = options.ml \
	  version.ml \
	  dynArray.mli dynArray.ml \
	  location.mli location.ml \
	  table.mli table.ml \
          class.mli class.ml \
          type.mli type.ml \
          scope.mli scope.ml \
	  tree.ml \
	  code.mli code.ml \
	  alwstd.ml \
	  codeGen.ml

SOURCES = $(ML) parser.mly lexer.mll aw2c.ml

DEP_MODULES = $(ML) parser.mli parser.ml lexer.ml aw2c.ml

test: build testprograms.ml testparsing.mll
#
# Test the lexer and parser:
	make -f Makefile.testparsing
	./testparsing --test expressions  Tests/parser-lexing*.dat 
	./testparsing --test expressions  Tests/parser-expressions*.dat 
	./testparsing --test expressions  Tests/parser-statements*.dat 
	./testparsing --test declarations Tests/parser-declarations*.dat 
#
# Run the test suite:
	ocaml ./testprograms.ml Tests/*.alw
	rm -f /tmp/a.out /tmp/compile /tmp/stdout /tmp/stderr
#
# Run the test programs:
	make -C Tests/Separate
	make -C Tests/SeparateC
	make -C Tests/OldParse
	make -C Tests/Argv
	make -C Tests/Tracing
#
	echo "ALL TESTS PASSED"


libalw.a: alw.o alwstd.o alwio.o alwexcept.o alwstr.o
	ar -cr libalw.a alw.o alwstd.o alwio.o alwexcept.o alwstr.o

alwio.c: scanner_state_machine.py alwio.c.input
	python scanner_state_machine.py

# ------------------------------------------------------------------------------

# Make a distribution tar file.
# This has to be done in a directory under 'darcs' version control.

# The distribution is named after the current directory, like a Darcs branch.

BRANCH=$(notdir $(PWD))

# VERSION contains is the branch name and date of the last Darcs
# record, it is always updated by this target.
# Email addresses are removed because this file is visible on the Web.

VERSION: version

version:
	echo "$(shell echo "`hostname`:`pwd` - `darcs changes | head -1`")" | sed -e 's/@.*//g' > VERSION


dist: $(BRANCH).tar.gz $(BRANCH)-windows-fun-pack.tgz


$(BRANCH).tar.gz: all test version
# 	echo "$(shell echo "`hostname`:`pwd` - `darcs changes | head -1`")" > VERSION
	rm -f $(BRANCH).tar.gz $(BRANCH).tar
	darcs dist --dist-name=$(BRANCH)
	gunzip $(BRANCH).tar.gz
	tar -C .. -rf aw2c.tar $(BRANCH)/VERSION
	gzip $(BRANCH).tar
	rm -f $(BRANCH).tar


# This is a cut-down, hacked distribution for Windows, see "README-windows.txt":

$(BRANCH)-windows-fun-pack.tgz: all test version
	MANWIDTH=75 man ./aw2c.1 > aw2c-manpage.txt
	cp aw2c.rst aw2c-manual.txt
	tar -czf aw2c-windows-fun-pack.tgz \
              README-windows.txt \
              aw2c-manpage.txt aw2c.html $(HTMLMAN) \
	      alw.h alwio.h alw.c alwstd.c alwio.c alwexcept.c alwstr.c \
              $(DEP_MODULES) \
              Tests/OldParse/parse.alw Tests/OldParse/expected.output Tests/OldParse/GRAMMAR
	rm aw2c-manpage.txt

# ------------------------------------------------------------------------------

documentation: aw2c.html aw2c.1

# The files required by the aw2c.html manual page:
HTMLMAN = aw2c-fish.png glyn.png aw2c.css html4css1.css

# Make the HTML aw2c manual. 'rst2html' is part of "Python Docutils".

aw2c.html: aw2c.rst $(HTMLMAN)
	rst2html --title="aw2c Manual" --stylesheet-path=aw2c.css --link-stylesheet aw2c.rst aw2c.html

# Make the man file.  Sed edits in up-to-date directory and release
# information.

aw2c.1: version aw2c.1.input
	sed -e 's!{{BINDIR}}!$(BINDIR)!g' \
	    -e 's!{{LIBDIR}}!$(LIBDIR)!g' \
	    -e 's!{{DOCDIR}}!$(DOCDIR)!g' \
	    -e 's!{{INCDIR}}!$(INCDIR)!g' \
	    -e 's!{{VERSION}}!$(shell cat VERSION)!g' \
	    -e 's!{{DATE}}!$(shell date +%Y-%m-%d)!g' \
	    < aw2c.1.input > aw2c.1

# ------------------------------------------------------------------------------

clean::
	make -f Makefile.testparsing clean
	make -C Tests/Separate clean
	make -C Tests/SeparateC clean
	make -C Tests/OldParse clean
	rm -f Tests/*.c Tests/*~
	rm -f alwio.c alwio.dot
	rm -f *.o *.a
	rm -f aw2c
	rm -f aw2c.1 aw2c.html aw2c-manpage.txt aw2c-manual.txt

# ------------------------------------------------------------------------------

# This does all the real work when compiling the Ocaml sources.
include $(OCAMLMAKEFILE)

#end
