/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2024, AdaCore                     *
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

#include "gnatcov_rts_c-strings.h"

/* This variation of gnatcov_rts_c-base_io does not rely on stdlib's fwrite
   and instead expects the user to define a putchar function through which the
   trace's content will be written.  */

extern int gnatcov_rts_putchar (int c);

void
gnatcov_rts_put_string (struct gnatcov_rts_string str)
{
  size_t i;
  for (i = 0; i < str.length; i++)
    {
      gnatcov_rts_putchar (str.str[i]);
    }
}
