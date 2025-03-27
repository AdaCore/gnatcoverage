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

#ifndef GNATCOV_RTS_C_MEMORY_H
#define GNATCOV_RTS_C_MEMORY_H

#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif

  /* Smallest subset of functions needed for memory manipulations (memcpy and
     memset). These functions must either be defined by the runtime (even if it
     is in a bareboard environment), or provided by the user.

     Note that we declare them ourselves, as the runtime may not provide the
     string.h header: older versions of light runtimes did not package newlib,
     and thus did not provide the string.h portable header.  Though the memcpy
     and memset functions are provided by the light runtime in this case.  */
  extern void *memcpy (void *__dest, const void *__src, size_t __n);
  extern void *memset (void *__s, int __c, size_t __n);

#ifdef __cplusplus
}
#endif
#endif
