AW2C=../../aw2c
LIBALW=../../libalw.a
ALWH_PATH=../..

CFLAGS=-I$(ALWH_PATH)
LDLIBS=-lm -lgc $(LIBALW)

.SUFFIXES: .c .o .alw
.PHONY: test clean

test : clean program
	./program > actual.output
	diff expected.output actual.output
	echo "testprograms: separate compilation test passed!"

program : program.c separate.c

%.c : %.alw  $(ALW_H)
	$(AW2C) -o $@ $<

clean:
	rm -f program program.o program.c separate.o separate.c actual.output
