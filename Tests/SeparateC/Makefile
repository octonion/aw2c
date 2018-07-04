AW2C=../../aw2c
LIBALW=../../libalw.a
ALWH_PATH=../..

CFLAGS=-I$(ALWH_PATH)
LDLIBS=-lm -lgc $(LIBALW)

.PHONY: test clean

test : clean program
	./program > actual.output
#	cat actual.output
	diff expected.output actual.output
	echo "testprograms: separate compilation of a C function passed!"

%.c : %.alw  $(ALW_H)
	$(AW2C) -o $@ $<

program : program.c cprocedure.c

clean:
	rm -f program *.o program.c actual.output
