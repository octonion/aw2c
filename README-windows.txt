The "Windows Funpack"
---------------------

This is the Makefile-less, garbage-collector-less distribution of aw2c. It is a hack to allow Carey Bloodsworth to compile aw2c with the MingW GCC compiler under Windows with minimal tool support. I don't reccomend using this unless you really thing know what you are doing. Almost all Aw2c programs require a garabge collector.

To compile the aw2c compiler, run this:

   ocamlc version.ml dynArray.mli dynArray.ml location.mli location.ml table.mli table.ml class.mli class.ml type.mli type.ml scope.mli scope.ml tree.ml code.mli code.ml alwstd.ml codeGen.ml parser.mli parser.ml lexer.ml aw2c.ml -o aw2c.exe

Replace "ocamlc" with "ocamlopt" to compile to native code. (The ocamlc version performs fine though.)

Test "aw2c" by compiling "Tests\OldParse\parse.alw" to C:

   aw2c Tests\OldParse\parse.alw -o parse.c

"parse.alw" is in directory "Tests/OldParse".

Compile the following files to produce the aw2c runtime library. (I'm not sure how that is done with MingGW.)

   alw.h alw.c alwstd.c alwio.c alwexcept.c alwstr.c

You must use the gcc flag -DALW_NO_LIBGC while you are still without a
Boehm GC library to link Algol W programs to.

Now try compiling parse.c into a binary. I do it like this:

   gcc -I. -lgc -lm parse.c libalw.a -o parse

You will have to adapt that to MingGW. "libalw.a" is the runtime
library. "alw.h" must be on the include path.

This seems to work too:

   gcc -DALW_NO_LIBGC -I. -lm alw.c alwstd.c alwio.c alwexcept.c alwstr.c parse.c -o parse

Try out "parse":

   parse < Tests\OldParse\GRAMMAR > parse.output

"parse.output" should be identical to "Tests\OldParse\expected.output", except for the trailing spaces.

You should be reasonably certain of having a working Algol W compiler at this point.

--Glyn
