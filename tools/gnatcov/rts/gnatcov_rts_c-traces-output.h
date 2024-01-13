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

#include "gnatcov_rts_c-buffers.h"
#include "gnatcov_rts_c-strings.h"

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

  /* Callback for trace writing routines. Write the N bytes starting at SOURCE
     to the OUTPUT stream (OUTPUT is just forwarded from
     gnatcov_rts_generic_write_trace_file).  Return 0 if the write was
     successful and return any non-zero value in case of error.  */
  typedef int (*gnatcov_rts_write_bytes_callback) (void *output,
						   const void *source,
						   unsigned n);

  /* Write a trace file to contain the given coverage BUFFERS_GROUPS to the
     OUTPUT stream using the WRITE_BYTES callback.  PROGRAM_NAME, EXEC_DATE and
     USER_DATA are included as metadata in the trace file.  Return 0 if the
     write was successful, and return any non-zero value in case of error.  */
  extern int gnatcov_rts_generic_write_trace_file (
    void *output,
    const struct gnatcov_rts_coverage_buffers_group_array *buffers_groups,
    struct gnatcov_rts_string program_name, uint64_t exec_date,
    struct gnatcov_rts_string user_data,
    gnatcov_rts_write_bytes_callback write_bytes);

#ifdef __cplusplus
}
#endif
