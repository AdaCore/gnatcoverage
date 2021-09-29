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

#include <stdlib.h>
#include <time.h>

#if defined(_WIN32)
#include <processthreadsapi.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

#include "gnatcov_rts_c_interface.h"

uint64_t
gnatcov_rts_time_to_uint64 (void)
{
  return (uint64_t)time (NULL);
}

uint64_t
gnatcov_rts_getpid (void)
{
#if defined(_WIN32)
  return (uint64_t)GetCurrentProcessId ();
#else
  return (uint64_t)getpid ();
#endif
}
