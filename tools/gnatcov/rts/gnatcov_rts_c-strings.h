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

#ifndef GNATCOV_RTS_C_STRINGS_H
#define GNATCOV_RTS_C_STRINGS_H

#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

  struct gnatcov_rts_string
  {
    const char *str;
    size_t length;
  };

/* Converts a string litteral to the corresponding gnatcov_rts_string struct
   value.  */
#ifdef __cplusplus
#define STR(x)                                                                \
  {                                                                           \
    x, sizeof (x) - 1                                                         \
  } // Avoid compound literal which is not valid C++
#else
#define STR(x)                                                                \
  (struct gnatcov_rts_string) { x, sizeof (x) - 1 }
#endif

#ifdef __cplusplus
}
#endif

#endif
