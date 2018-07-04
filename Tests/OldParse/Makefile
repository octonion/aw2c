AW2C=../../aw2c
LIBALW=../../libalw.a
ALWH_PATH=../..

CFLAGS=-I$(ALWH_PATH)
LDLIBS=-lm -lgc $(LIBALW)

.SUFFIXES: .c .o .alw
.PHONY: test clean

test: clean parse
	./parse < GRAMMAR > actual.output
	diff expected.output actual.output
	echo "testprograms: run of PARSE passed!"

parse: parse.c

parse.c: parse.alw
	$(AW2C) parse.alw -o parse.c

parse.alw:  parse.alw.orig
#	The timing is different every run, so the timing statements have to go:
	sed -e 's/WRITE("Time:.*;//g' < parse.alw.orig > parse.alw

clean:
	rm -f parse parse.c parse.alw parse.o actual.output
