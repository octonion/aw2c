AW2C=../../aw2c
LIBALW=../../libalw.a
ALWH_PATH=../..

CFLAGS=-I$(ALWH_PATH)
LDLIBS=-lm -lgc $(LIBALW)

.SUFFIXES: .c .o .alw
.PHONY: test clean

test : clean program
	./program Argument1 Arg2 'argument 3' > actual.output
	diff expected.output actual.output
	echo "testprograms: argv test passed!"

program : program.c

%.c : %.alw  $(ALW_H)
	$(AW2C) -o $@ $<

clean:
	rm -f program program.o program.c actual.output
