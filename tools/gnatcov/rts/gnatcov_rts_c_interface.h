/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2020-2021, AdaCore                     *
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

#include <stdint.h>

/* The C "time" function can return integers of
   different size depending on the platform.
   Here, we ensure that the returned result will be an
   expected long long (a 64 bit integer). It will thus
   be compatible with 32 bit as well as 64 bit architectures.
   It also ensures compatibility with Ada Long_Long_Integer
   standard type. */
extern uint64_t gnatcov_rts_time_to_uint64 (void);

/* Return the current process ID as an unsigned 64-bit integer. */
extern uint64_t gnatcov_rts_getpid (void);
