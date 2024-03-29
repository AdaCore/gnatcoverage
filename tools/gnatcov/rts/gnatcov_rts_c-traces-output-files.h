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
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

/* Default name of the environment variable which controls the default
   filename for source traces: see the gnatcov_rts_default_trace_filename
   function below.  */
#define GNATCOV_RTS_DEFAULT_TRACE_FILENAME_ENV_VAR "GNATCOV_TRACE_FILE"

  /* Return the default name of the trace file to write.  The returned name
     must be manually freed.

     If the ENV_VAR environment variable is not defined or empty, return:

     * "PREFIX.srctrace" if SIMPLE is True.

     * "PREFIX-TAG-PID-CLOCK-COUNT.srctrace" if SIMPLE is False (PID is the
       current process ID, CLOCK is the execution timestamp, COUNT is the
       number of previously dumped traces by the program).  The "-TAG" part
       is omitted if TAG is the empty string.

     If the ENV_VAR environment variable is defined and not empty, then:

     * if it ends with "/" or "\", consider it contains the name of a
     directory: use the algorithm described above to compute the basename and
     return the filename in that directory;

     * otherwise, just return the content of that environment variable.  */
  extern char *gnatcov_rts_default_trace_filename (const char *env_var,
                                                   const char *prefix,
                                                   const char *tag,
                                                   unsigned simple);

  /* Write a trace file in FILENAME to contain the data in BUFFERS_GROUPS.

     PROGRAM_NAME, EXEC_DATE, and USER_DATA are used to fill the
     corresponding metadata in the written trace file.

     EXEC_DATE is given to produce the timestamp. Use the current
     time by default.

     If the source trace file creation fails, print an error message on the
     standard error.  */
  extern void gnatcov_rts_write_trace_file (
    const struct gnatcov_rts_coverage_buffers_group_array *buffers_groups,
    const char *filename, struct gnatcov_rts_string program_name,
    uint64_t exec_date, struct gnatcov_rts_string user_data);

#ifdef __cplusplus
}
#endif
