AW2C=../../aw2c
LIBALW=../../libalw.a
ALWH_PATH=../..

CFLAGS=-I$(ALWH_PATH)
LDLIBS=-lm -lgc $(LIBALW)

.SUFFIXES: .c .o .alw
.PHONY: test clean

test : clean program
	./program 2> actual.output
	diff expected.output actual.output
	echo "tracing test passed!"

program : program.c tracing.c 

%.c : %.alw  $(ALW_H)
	$(AW2C) --trace -o $@ $<

clean:
	rm -f tracing.o program program.o program.c actual.output
