/* alwio.h -- this exposes the alwio Printer and Scanner types for C programmers

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

#include <stdio.h>
#include <stdbool.h>


#define Scanner_BUFSIZE 512


typedef struct {
    FILE *input;
    const char *input_name;
    bool eof;
    int state;
    int line;
    int column;
    int start_line;
    alw_chr buffer[Scanner_BUFSIZE];
    int buflen;
} alw_Scanner;


typedef struct {
    FILE *output;
    const char *output_name;
    int page;         /* The page in the printer, first page = 1 */
    int line;         /* The line the virtual printhead is in, first line = 1 */
    int column;       /* The column the virtual printhead should be in, first column = 1 */
    int true_column;  /* The column the printhead is really in, the printhead stops short when
                         printing string columns and the spaces after fields, those will not be
                         printed if they appear at the end of a line. */
} alw_Printer;


extern alw_Scanner alw_stdin_scanner;   /* Input scanner reading from the standard input */
extern alw_Scanner *alw_active_scanner; /* The scanner in use. */

extern alw_Printer alw_stdout_printer;   /* Printer writing to the standard output */
extern alw_Printer *alw_active_printer; /* The printer in use. */


void alw_Scanner_initialize (alw_Scanner *scanner, FILE *file, const char *name);

void alw_Printer_initialize (alw_Printer *printer, FILE *file, const char *name);


/* end */
