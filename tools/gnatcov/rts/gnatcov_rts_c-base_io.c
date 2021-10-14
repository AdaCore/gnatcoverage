/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2022, AdaCore                     *
 *                                                                          *
 * GNATcoverage is free software; you can redistribute it and/or modify it  *
 * under terms of the GNU General Public License as published by the  Free  *
 * Software  Foundation;  either version 3,  or (at your option) any later  *
 * version. This software is distributed in the hope that it will be useful *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>

#include "gnatcov_rts_c_strings.h"

/* The libc implementation always provides fwrite / putchar.  Use these
   functions to print to the standard output.  */

int
gnatcov_rts_puts (gnatcov_rts_string str)
{
  fwrite (str.str, 1, str.length, stdout);
  fwrite ("\n", 1, 1, stdout);
  return 0;
}

extern int
gnatcov_rts_putchar (int c)
{
  return putchar (c);
}
