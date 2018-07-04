/* alwexception.c -- the predefined record class 'Exception'

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
#include <stdlib.h>


void *unfl;
void *ovfl;
void *divzero;
void *intovfl;
void *intdivzero;
void *sqrterr;
void *experr;
void *lnlogerr;
void *sincoserr;
void *endfile;


struct exception {
  const char *_class;
  int _number;
  int xcpnoted;
  int xcplimit;
  int xcpaction;
  int xcpmark;
  alw_str xcpmsg;
};


const char * const alw_0000_exception = "exception";

void *
exception ( alw_loc loc, 
            int xcpnoted,
            int xcplimit,
            int xcpaction,
            int xcpmark,
            alw_str xcpmsg )
{
  struct exception *ref = (struct exception *)alw_allocate_record(loc, sizeof(struct exception));
  ref->_class = alw_0000_exception;
  ref->_number = alw_record_counter++;
  ref->xcpnoted = xcpnoted;
  ref->xcplimit = xcplimit;
  ref->xcpaction = xcpaction;
  ref->xcpmark = xcpmark;
  ref->xcpmsg = alw_str_new(xcpmsg, 64, 64);
  return (void *)ref;
}

int *
xcpnoted (alw_loc loc, void *ref) 
{
  alw_ref_field_check(loc, ref, alw_0000_exception, "xcpnoted");
  return &((struct exception *)ref)->xcpnoted;
}


int *
xcplimit (alw_loc loc, void *ref)
{
  alw_ref_field_check(loc, ref, alw_0000_exception, "xcplimit");
  return &((struct exception *)ref)->xcplimit;
}


int *
xcpaction (alw_loc loc, void *ref) 
{
  alw_ref_field_check(loc, ref, alw_0000_exception, "xcpaction");
  return &((struct exception *)ref)->xcpaction;
}


int *
xcpmark (alw_loc loc, void *ref) 
{
  alw_ref_field_check(loc, ref, alw_0000_exception, "xcpmark");
  return &((struct exception *)ref)->xcpmark;
}


alw_str
xcpmsg (alw_loc loc, void *ref) 
{
  alw_ref_field_check(loc, ref, alw_0000_exception, "xcpmsg");
  return ((struct exception *)ref)->xcpmsg;
}


void
alw_process_exception (alw_loc loc, void *condition)
{
  if (condition) {
    *xcpnoted(loc, condition) = 1;
    (*xcplimit(loc, condition))--;
    if (*xcplimit(loc, condition) < 0 || *xcpmark(loc, condition)) {
      char msg[65];
      alw_str_cpy(msg, 64, xcpmsg(loc, condition), 64); msg[64] = '\0';
      alw_warning(loc, msg);
    }
    if (*xcplimit(loc, condition) < 0)
      alw_exit(loc, EXIT_FAILURE);
  }
}


void
alw_init_exceptions (alw_loc loc)
{
  unfl       = NULL;
  alw_record_counter = -10; /* Predeclared records will have negative record numbers. */
  ovfl       = exception(loc, 0, 0, 0, 1, "Floating-point exponent overflow.                               ");
  divzero    = exception(loc, 0, 0, 0, 1, "Floating-point division by zero.                                ");
  intovfl    = exception(loc, 0, 0, 0, 1, "Integer overflow.                                               ");
  intdivzero = exception(loc, 0, 0, 0, 1, "Integer division by zero.                                       ");
  sqrterr    = exception(loc, 0, 0, 0, 1, "Negative argument for SQRT or LONGSQRT.                         ");
  experr     = exception(loc, 0, 0, 0, 1, "Argument of EXP or LONGEXP out of domain.                       ");
  lnlogerr   = exception(loc, 0, 0, 0, 1, "Argument of LN, LOG, LONGLN or LONGLOG out of domain.           ");
  sincoserr  = exception(loc, 0, 0, 0, 1, "Argument of SIN, COS, LONGSIN or LONGCOS out of domain.         ");
  endfile    = exception(loc, 0, 0, 0, 1, "Unexpected end of input.                                        ");
  alw_record_counter = 0;
}


/* end */
