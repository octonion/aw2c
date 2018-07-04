/* alw.h -- Algol W support library 

--

This file is part of aw2c. Copyright 2008 Glyn Webster.

This file is free software: you can redistribute it and/or modify it
under the terms of the GNU Limited General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aw2c is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU Limited General Public
License along with aw2c.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifndef __ALW_H
#define __ALW_H


/* Entering and exiting the program, runtime messages. - - - - - - - - - - - - - - - - - - - - - - - - - -  */


/* Represents a location in an Algol W source file. */
typedef struct alw_LOC {
  const char *file; 
  int line; 
  int column;
} *alw_loc;

#define alw_at(line, column) (&(struct alw_LOC){alw_src, line, column}) 
/* alw_src is a string variable valid at the point of call */

#define alw_HERE alw_at(__LINE__,0)  /* A macro for alw_loc locations for use in inline C. */



/* Enter the program and initialize the Algol W libraries. */
void alw_init (alw_loc loc);


/* Finalize the Algol W libraries and exit the program. */
void alw_exit (alw_loc loc, int exitcode);


/* Issue a run-time error, reporting the Algol W source location, and halt. */
void alw_error(alw_loc l, const char *format, ...);

/* Issue a run-time error, reporting the Algol W source location. Don't halt. */
void alw_warning(alw_loc l, const char *format, ...);

/* Process an "Exceptional Condition" (but do not provide a default value). */
void processexception (alw_loc loc, void *condition);

/* These read environment variables. */
/* An out-of-range integer or an unrecognizable boolean flag is a runtime error. */
int alw_env_int  (alw_loc l, const char *variable, int default_, int max, int min);
int alw_env_bool (alw_loc l, const char *variable, int default_);



/* References. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */


/* The gcc compiler flag -D ALW_NO_REFERENCE_CHECKS turns off array bounds checking. */
#ifdef ALW_NO_REFERENCE_CHECKS
#define alw_ref_cast(loc, ref, classes) (ref)
#define alw_ref_field_check(loc, ref, class, field_name)
#else

struct alw_any_record {
    const char *_class; 
    int _number;
};

#define alw_class(ref) (((struct alw_any_record *)ref)->_class)
#define alw_record_number(ref) (((struct alw_any_record *)ref)->_number)

int alw_record_counter;

/* This is used in some reference assignments and actual parameters;
   it returns reference 'ref' if it refers to a record that belongs
   in 'classes', otherwise it raises a run-time reference type error.
   This is only called in places where such an error is possible.  */
#define alw_ref_cast(loc, ref, classes...) (alw_ref_cast_check(loc, ref, (const char *[]){classes, (const char *)0}))
void *alw_ref_cast_check (alw_loc loc, void *ref, const char **classes);

/* This is uswed in field designator functions; it raises a run-time
   reference type error if reference 'ref' does not belong in
   'class'. */
void alw_ref_field_check (alw_loc loc, void *ref, const char *class, const char *field_name);

#endif

/* The IS operator. */
int alw_is (void *ref, const char *class);

/* This allocates garbage collected storage for a record. */
void *alw_allocate_record(alw_loc l, int size);



/* Arrays. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* The gcc compiler flag -D ALW_NO_ARRAY_CHECKS turns off array bounds checking. */
#ifdef ALW_NO_ARRAY_CHECKS
#define alw_array_bounds_check(a, l, d)
#define alw_array_range_check(a, d)
#else

/* On creation, check that a dimension's bounds valid. */
/* A lower bound one less than its upper bound is allowed, it creates an empty array. */
#define alw_array_bounds_check(a, alw_src_line, d)\
    if (_##a##_upb##d - _##a##_lwb##d + 1 < 0) { \
        alw_array_bounds_error(alw_src_line, #a, d, _##a##_lwb##d, _##a##_upb##d);\
    }
void alw_array_bounds_error(alw_loc l, const char *array, int subscript, int lwb, int upb);


/* On access, check that a subscript is within its dimension's bounds. */
#define alw_array_range_check(a, d)\
    if (!(_sub##d >= _##a##_lwb##d && _sub##d <= _##a##_upb##d)) {        \
        alw_array_range_error(loc, #a, d, _##a##_lwb##d, _##a##_upb##d, _sub##d);\
    }
void alw_array_range_error(alw_loc l, const char *array, int subscript, int lwb, int upb, int sub);

#endif



/* Statements. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* Perform an assertion. (Gives the Algol W source code location on failure.) */
void alw_assert(alw_loc l, int condition);


/* Check that the STEP of a FOR statement is not zero. (An unending loop.) */
void alw_check_for_step(alw_loc l, int for_step);


/* Reports an error when CASE selector is out of range. */
void alw_case_range_error(alw_loc l, int selector);



/* Arithmetic. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* The gcc compiler flag -D ALW_NO_ARITHMETIC_CHECKS turns off arithmetic bounds checking. */
#ifdef ALW_NO_ARITHMETIC_CHECKS
#define alw_shl(l, bits, shift) ((bits) << (shift))
#define alw_shr(l, bits, shift) ((bits) >> (shift))
#define alw_div(l, a, b) ((a) / (b))
#define alw_rem(l, a, b) ((a) % (b))
#define alw_rdiv(l, a, b) ((a) / (b))
#define alw_cdiv(l, a, b) ((a) / (b))
#else


/* Negative shifts are a runtime error. */
unsigned int alw_shl(alw_loc l, unsigned int bits, int shift);
unsigned int alw_shr(alw_loc l, unsigned int bits, int shift);


/* Divide-by-zero is a runtime error. */
int alw_div(alw_loc l, int a, int b);
int alw_rem(alw_loc l, int a, int b);
double alw_rdiv(alw_loc loc, double dividend, double divisor);
_Complex double alw_cdiv(alw_loc loc, _Complex double dividend, _Complex double divisor);

/* The ABS operator */
int alw_abs(int i);
double alw_fabs(double r);
_Complex double alw_cabs(_Complex double x);

/* The ** operator */
double alw_rpwr(alw_loc l, double r, int n);
_Complex double alw_cpwr(alw_loc l, _Complex double x, int n);

#endif


/* Strings. - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Algol W strings are pointers to arrays of characters on the heap.
   They are pointers because A2WC is designed to treat all Algol W
   simple values as C rvalues, character arrays cannot be rvalues.

   alw_strs are not zero-terminated, their lengths should be known
   at all times.

   Each alw_str has its own array, except substring designators
   (results of 'alw_str_sub') those point into other alw_str's
   arrays. 
*/
typedef unsigned char alw_chr;
typedef alw_chr *alw_str;

extern alw_chr alw_ebcdic_of_latin1 [256];
extern alw_chr alw_latin1_of_ebcdic [256];


/* Allocate a fresh array, initially full of spaces. */
alw_str alw_str_var (int length);


/* Allocate a fresh copy of 'src', padded with spaces if necessary.
   'src' may be a C string.
*/
alw_str alw_str_new (const alw_str src, int srclen, int dstlen);
alw_str alw_str_new_c (const alw_chr src, int length);


/* Perform 'dst := src', return 'src': */
alw_str alw_str_cpy (alw_str dst, int dstlen, const alw_str src, int srclen);
alw_chr alw_str_cpy_sc (alw_str dst, int dstlen, alw_chr src);


/* Return the address of 'src(index|len)'.  There is a runtime error
   if the substring is not completely in the bounds of the string.
*/
alw_str alw_str_sub (alw_loc loc, const alw_str src, int srclen, int index, int length);


/* Compare two strings.  Spaces at the ends of strings do not count in
   static string comparisions. */
int alw_str_cmp (const alw_str str1, int str1len, const alw_str str2, int str2len);
int alw_str_cmp_cs (alw_chr c1, const alw_str str2, int str2len);
int alw_str_cmp_sc (const alw_str str1, int str1len, alw_chr c2);
int alw_str_cmp_cc (alw_chr c1, alw_chr c2);

/* Initialize a string by filling it with spaces. */
void alw_str_init (alw_str dst, int dstlen);



/* The standard procedure library - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


void alw_init_alwstd (void);

/* These must have Type headers in alwstd.ml */

/* They must have individual function definitions, not be macros,
   because their identifiers can be redefined within the Algol W
   program.     XXX what? */

int truncate(double r);

int entier(double r);

double roundtoreal(double r);

int round_(double r);

int odd_(int i);

unsigned int bitstring(int i);

int number(unsigned int bits);
 
int decode (alw_chr s);
alw_chr code(int i);

double imagpart(_Complex double x);
double realpart(_Complex double x);
_Complex double imag(double r);
double longimagpart(_Complex double x);
double longrealpart(_Complex double x);
_Complex double longimag(double r);


alw_str base10(double r);
alw_str base16(double r);
alw_str longbase10(double r);
alw_str longbase16(double r);

alw_str intbase10(int r);
alw_str intbase16(int r);

int maxinteger;
double pi;
double epsilon;
double longepsilon;
double maxreal;

double alw_sqrt (alw_loc, double);
double alw_exp (alw_loc, double);
double alw_ln (alw_loc, double);
double alw_log (alw_loc, double);
double alw_sin (alw_loc, double);
double alw_cos (alw_loc, double);
double alw_arctan (alw_loc, double);
#define alw_longsqrt alw_sqrt
#define alw_longexp alw_exp
#define alw_longln alw_ln
#define alw_longlog alw_log
#define alw_longsin alw_sin
#define alw_longcos alw_cos
#define alw_longarctan alw_arctan

int time_ (int n);



/* Exceptional conditions. - - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void alw_init_exceptions (alw_loc loc);

extern const char * const alw_0000_exception;  /* Exception class. This is global to the whole 
                                                  program, not local to the individual C files. */

void *exception( alw_loc loc, 
                 int xcpnoted, 
                 int xcplimit, 
                 int xcpaction, 
                 int xcpmark, 
                 alw_str  xcpmsg );

int *xcpnoted (alw_loc loc, void *ref);
int *xcplimit (alw_loc loc, void *ref);
int *xcpaction (alw_loc loc, void *ref);
int *xcpmark (alw_loc loc, void *ref);
alw_str xcpmsg (alw_loc loc, void *ref);

extern void *ovfl;
extern void *unfl;
extern void *divzero;
extern void *intovfl;
extern void *intdivzero;
extern void *sqrterr;
extern void *experr;
extern void *lnlogerr;
extern void *sincoserr;
extern void *endfile;

void alw_process_exception (alw_loc loc, void *condition);
/* The PROCESSEXCEPTION procedure described in section 8.5,
   except that supplying a default value is the responsibility of the
   caller. */



/* The Input/Output System - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */


/* Initialize the Input/Output System. This needs to be called at the
   start of any Algol W program.
 */
void alw_init_alwio (alw_loc l);


/* Shutdown the Input/Output System. This needs to be called at least
   the end of any Algol W program. (Only the first call will have any
   effect.)
 */
void alw_exit_alwio (alw_loc l);


/* Editing variables. 

   These control the formatting of parameters to WRITE statements.
   Values over 132 are silently treated as 132.

   Width of a INTEGER field                  I_W        14
   Number of spaces after a field            S_W         2
   Width of a REAL field                     R_W        14
   Number of decimal places in a REAL field  R_D         0
   REAL number output format                 R_FORMAT   "F"
*/
extern int   i_w;
extern int   s_w;
extern int   r_w;
extern int   r_d;
extern alw_chr r_format;  /* this is a STRING(1) */

/* The editing variables are saved at the start of an standard procedure
   statement and restored afterwards, c.f. section 7.9.4.  
 */

typedef struct {
    int   i_w;
    int   s_w;
    int   r_w;
    int   r_d;
    alw_chr r_format;
} alw_Editing_t;

void alw_Editing_save (alw_Editing_t* state);
void alw_Editing_restore (alw_Editing_t* state);


/* Actions for IOCONTROL statement parameters. 

   There will be a runtime error if the page estimate is exceeded by a
   line or page break instruction, or if the argument to
   'alw_iocontrol' is not a valid IOCONTROL control code.
*/
void alw_iocontrol (alw_loc l, int code);


/* Actions for WRITE or WRITEON actual parameters.  There will be a
   runtime error if the "page estimate" is exceeded. (See the aw2c
   manual.)  */

void alw_write_integer      (alw_loc l, int i);
void alw_write_real         (alw_loc l, double r);
void alw_write_long_real    (alw_loc l, double r);
void alw_write_complex      (alw_loc l, _Complex double r);
void alw_write_long_complex (alw_loc l, _Complex double r);
void alw_write_logical      (alw_loc l, int b);
void alw_write_bits         (alw_loc l, unsigned int b);
void alw_write_string       (alw_loc l, alw_str s, int length);
void alw_write_char         (alw_loc l, alw_chr c);
void alw_write_reference    (alw_loc loc, void *ref);


/* Actions for READ, READON and READCARD actual parameters. */

void alw_read_integer  (alw_loc l, int *i);
void alw_read_real     (alw_loc l, double *r);
void alw_read_complex  (alw_loc l, _Complex double *r);
void alw_read_logical  (alw_loc l, int *b);
void alw_read_bits     (alw_loc l, unsigned int *b);
void alw_read_string   (alw_loc l, alw_str s, int length);
void alw_read_char     (alw_loc l, alw_chr *c);
void alw_readcard      (alw_loc l, alw_str string, int length);
void alw_readcard_char (alw_loc l, alw_chr *c);


#endif

/* end */
