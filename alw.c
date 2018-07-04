/* alw.c -- Algol W support library 

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

#include "alw.h"

#ifdef ALW_NO_LIBGC
#include <malloc.h>
#define GC_MALLOC malloc
#else
#include <gc/gc.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <limits.h>
#include <fenv.h>

#include <complex.h>

extern const char *alw_src;  /* The source file name. The generated code supplies this. */
extern const char * const alw_class_names[];  /* the names of the records, indexed by class number. */
                                              /* XXX this will confuse external procedures. */ 

void
alw_init (alw_loc loc)
{
    alw_record_counter = 0;
    alw_init_alwstd();
    alw_init_exceptions(loc);
    alw_init_alwio(loc);
}


/* Shut down all parts of the AW2C runtime. */
static 
void
alw_finalize (alw_loc loc)
{
    alw_exit_alwio(loc);  /* Currently this all there is to it. */
}


void
alw_exit (alw_loc loc, int code)
{
    alw_finalize(loc);
    exit(code);
}


void
alw_error (alw_loc l, const char *format, ...)
{
  va_list args;

  /* Flush the standard output first so that the error message appears
     in a sensible place. */
  alw_finalize(l);

  va_start(args, format);
  if (l)
      fprintf(stderr, "%s:%d:%d: ", l->file, l->line, l->column + 1);
  else
      fprintf(stderr, "<unknown location>: ");
  vfprintf(stderr, format, args);
  va_end(args);

  exit(EXIT_FAILURE);
}


void
alw_warning (alw_loc l, const char *format, ...)
{
  va_list args;
  va_start(args, format);
  if (l)
      fprintf(stderr, "%s:%d:%d: ", l->file, l->line, l->column + 1);
  else
      fprintf(stderr, "<unknown location>: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  va_end(args);
}


int
alw_env_int(alw_loc l, const char *name, int default_, int min, int max)
{
  char *var, *tail;
  int val;

  var = getenv(name);
  if (!var) 
    return default_;

  val = strtol (var, &tail, 10);
  if (tail == var || val > max || val < min)
    alw_error(l, "Expected an integer between %d and %d in system variable %s.", min, max, name);

  return val;
}


int
alw_env_bool (alw_loc l, const char *name, int default_)
{
  char *var;

  var = getenv(name);
  if (!var) 
    return default_;

  if(strcasecmp(var, "on") == 0) return 1;
  if(strcasecmp(var, "true") == 0) return 1;
  if(strcasecmp(var, "t") == 0) return 1;
  if(strcasecmp(var, "1") == 0) return 1;
  if(strcasecmp(var, "yes") == 0) return 1;
  if(strcasecmp(var, "y") == 0) return 1;
  
  if(strcasecmp(var, "off") == 0) return 0;
  if(strcasecmp(var, "0") == 0) return 0;
  if(strcasecmp(var, "false") == 0) return 0;
  if(strcasecmp(var, "f") == 0) return 0;
  if(strcasecmp(var, "no") == 0) return 0;
  if(strcasecmp(var, "n") == 0) return 0;

  /* You've had enough chances. */
  alw_error(l, "Expected a true or false value in system variable %s.", name);
}



/* Records and references ------------------------------------------------------------ */


int alw_record_counter;


void *
alw_allocate_record(alw_loc l, int size)
{
  void *record;

  record = GC_MALLOC(size);
  if (record)
    return record;
  else
    alw_error(l, "Could not allocate record %i: Out of memory!\n", alw_record_counter + 1);
}


int
alw_is (void *ref, const char *class) 
{
  return ref && alw_class(ref) == class;  /* i.e. pointer is not NULL and points to record of right class */
}


void 
alw_ref_field_check (alw_loc loc, void *ref, const char *class, const char *field_name)
{
  if (!ref)
    alw_error(loc, "reference error: tried to find field %s of a NULL reference\n", field_name);
  if (!alw_is(ref, class))
    alw_error( loc, "reference error: tried to find field %s of a REFERENCE(%s)\n",
               field_name, 
               alw_class(ref) );
}


/* Converts a zero-termintes array of record class numbers into the
   Algol W name for a reference type.  */
/* This only gets used by the function below. */
static
char *
alw_string_of_ref (const char **classes)
{
  static char s[512];
  const char **pclass;
  
  assert(*classes != 0);
  strcpy(s, "a REFERENCE(");
  strcat(s, *classes);
  for (pclass = classes + 1; *pclass != 0; ++pclass) {
    strcat(s, ", ");
    strcat(s, *pclass);
  }
  strcat(s, ")");
  return s;
}


void *
alw_ref_cast_check (alw_loc loc, void *ref, const char **classes)
{
  const char **pclass;

  if (ref == NULL)
      return NULL;
  for (pclass = classes; *pclass != 0; ++pclass)
    if (alw_class(ref) == *pclass) 
      return ref;
  alw_error( loc, "reference error: %s cannot be made to refer to a '%s' record.\n",
             alw_string_of_ref(classes),
             alw_class(ref) );
}



/* Arrays -------------------------------------------------------------------------------- */


void
alw_array_range_error(alw_loc l, const char *array, int subscript, int lwb, int upb, int sub)
{
    if (upb - lwb < 1)
        alw_error(l, "array subscript error: subscript %d of '%s' = %d, of empty array range (%d::%d)\n",
                  subscript, array, sub, lwb, upb);
    else
        alw_error(l, "array subscript error: subscript %d of '%s' = %d, outside the range (%d::%d)\n",
                  subscript, array, sub, lwb, upb);
}


void
alw_array_bounds_error(alw_loc l, const char *array, int subscript, int lwb, int upb)
{
    alw_error(l, "array bounds error: bound %d of '%s' is (%d::%d) here\n",
	    subscript, array, lwb, upb);
}


void
alw_assert(alw_loc l, int condition)
{
    if(! condition) {
        alw_error(l, "assertion failure\n");
    }
}


void
alw_check_for_step(alw_loc l, int for_step)
{
  if (for_step == 0)  alw_error(l, "FOR step of 0\n");
}


void
alw_case_range_error(alw_loc l, int selector)
{
  alw_error(l, "CASE range error: selector is %d\n", selector);
}


unsigned int
alw_shl(alw_loc l, unsigned int bits, int shift)
{
  if (shift >= 0)
    return bits << shift;
  else
    alw_error(l, "negative shift operand to SHL\n");
}


unsigned int
alw_shr(alw_loc l, unsigned int bits, int shift)
{
  if (shift >= 0)
    return bits >> shift;
  else
    alw_error(l, "negative shift operand to SHR\n");
}


int
alw_div(alw_loc loc, int dividend, int  divisor)
{
  if (divisor != 0)
    return dividend / divisor;
  else if (intdivzero == NULL)
    return dividend;
  else {
    alw_process_exception(loc, intdivzero);
    return dividend;
  }
}


int
alw_rem(alw_loc loc, int dividend, int  divisor)
{
  if (divisor != 0)
    return dividend % divisor;
  else if (intdivzero == NULL)
    return dividend;
  else {
    alw_process_exception(loc, intdivzero);
    return dividend;
  }
}


double
alw_rdiv(alw_loc loc, double dividend, double divisor)
{
  double quotient;
  feclearexcept(FE_DIVBYZERO);
  quotient = dividend / divisor;
  if (fetestexcept(FE_DIVBYZERO)) {
    feclearexcept(FE_DIVBYZERO);
    alw_process_exception (loc, divzero);
    if (divzero) 
      switch (*xcpaction(loc, divzero)) {
      case 1: return maxreal;
      case 2: return 0.0;
      default: return dividend;
      }
    else
      return dividend;
  }
  else
    return quotient;
}


_Complex double
alw_cdiv(alw_loc loc, _Complex double dividend, _Complex double divisor)
{
  _Complex double quotient;
  feclearexcept(FE_DIVBYZERO);
  quotient = dividend / divisor;
  if (fetestexcept(FE_DIVBYZERO)) {
    feclearexcept(FE_DIVBYZERO);
    alw_process_exception (loc, divzero);
    if (divzero) 
      switch (*xcpaction(loc, divzero)) {
      case 1: return maxreal;
      case 2: return 0.0;
      default: return dividend;
      }
    else
      return dividend;
  }
  else
    return quotient;
}


int 
alw_abs(int i)
{
  return abs(i);
}


double 
alw_fabs(double r)
{
  return fabs(r);
}


_Complex double alw_cabs(_Complex double x)
{
  return cabs(x);
}


static
double 
rpwr_loop (double x, int n)
{
  double result = 1.0;
  while (n) {
    if (n & 1) {
      result *= x;
      n -= 1;
    }
    x *= x;
    n /= 2;
  }
  return result;
}


double
alw_rpwr (alw_loc l, double x, int n)
{
  if (x == 0.0 && n < 0)
      alw_error(l, "Exponent operator division by zero: 0 ** %d", n);
  else if (n == 0)
      return 1.0;
  else if (n >= 0)
    return rpwr_loop(x, n);
  else
    return 1.0 / rpwr_loop(x, -n);
}
  

static
_Complex double 
cpwr_loop (_Complex double x, int n)
{
  _Complex double result = 1.0;
  while (n) {
    if (n & 1) {
      result *= x;
      n -= 1;
    }
    x *= x;
    n /= 2;
  }
  return result;
}


 _Complex double
alw_cpwr (alw_loc l, _Complex double x, int n)
{
  if (x == 0.0 && n < 0)
      alw_error(l, "Exponent operator division by zero: 0 ** %d", n);
  else if (n == 0)
      return 1.0;
  else if (n >= 0)
    return cpwr_loop(x, n);
  else
    return 1.0 / cpwr_loop(x, -n);
}
  

/* end */
