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

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnatcov_rts_c-os_interface.h"
#include "gnatcov_rts_c-traces-output-files.h"
#include "gnatcov_rts_c-traces-output.h"

static int
write_bytes (void *output, void *bytes, unsigned count)
{
  FILE *file = (FILE *) output;
  fwrite (bytes, count, 1, file);
  return 0;
}

/* Return the hex string representation of a uint64_t */
static char *
hex_str (uint64_t value)
{
  char *res = (char *) malloc (16);
  sprintf (res, "%" PRIx64, value);
  return res;
}

/* Return the concatenation of the given strings */
char *
concat (const char *s1, ...)
{
  va_list args;
  const char *s;

  char *p, *result;
  size_t l, m, n;

  m = n = strlen (s1);

  /* Compute length of result string.  */

  va_start (args, s1);
  while ((s = va_arg (args, char *)))
    {
      m += strlen (s);
    }
  va_end (args);

  result = (char *) malloc (m + 1);

  /* Then, concatenate all the strings in the result buffer.  */

  memcpy (p = result, s1, n);
  p += n;
  va_start (args, s1);
  while ((s = va_arg (args, char *)))
    {
      l = strlen (s);
      memcpy (p, s, l);
      p += l;
    }
  va_end (args);

  /* Null-terminate the result string.  */

  *p = 0;

  return result;
}

/* Helper for gnatcov_rts_default_trace_filename, to be called when the
   environment variable does not provide the source trace filename. Return the
   basename for the source trace file.  See the documentation of
   gnatcov_rts_default_trace_filename for more information.  */
char *
gnatcov_rts_default_trace_basename (char *prefix, char *tag, unsigned simple)
{
  char *extension = ".srctrace";
  if (simple)
    return concat (prefix, extension, NULL);

  char *pid = hex_str (gnatcov_rts_getpid ());
  char *time = hex_str (gnatcov_rts_time_to_uint64 ());
  char *suffix = concat ("-", pid, "-", time, extension, NULL);
  free (pid);
  free (time);

  char *res;

  if (!tag[0])
    res = concat (prefix, suffix, NULL);
  else
    res = concat (prefix, "-", tag, suffix, NULL);
  free (suffix);
  return res;
}

/* See gnatcov_rts_cc-traces-output-files.h.  */
char *
gnatcov_rts_default_trace_filename (char *env_var, char *prefix, char *tag,
                                    unsigned simple)
{
  char *env_trace_filename = getenv (env_var);

  if (!env_trace_filename)
    return gnatcov_rts_default_trace_basename (prefix, tag, simple);
  else
    {
      /* The caller is supposed to free the returned string, so create a
         copy.  */
      env_trace_filename = concat (env_trace_filename, NULL);

      /* If the filename ends with a directory separator, consider that it
         refers to a directory: in that case return a filename inside it.  */
      size_t length = strlen (env_trace_filename);
      if (length > 0
          && (env_trace_filename[length - 1] == '/'
              || env_trace_filename[length - 1] == '\\'))
        {
          char *basename
            = gnatcov_rts_default_trace_basename (prefix, tag, simple);
          char *res = concat (env_trace_filename, basename, NULL);
          free (env_trace_filename);
          free (basename);
          return res;
        }
      else
        return env_trace_filename;
    }
}

/* See gnatcov_rts_c-traces-output-files.h.  */
int
gnatcov_rts_write_trace_file (gnatcov_rts_unit_coverage_buffers_array *buffers,
                              char *filename, gnatcov_rts_string program_name,
                              uint64_t exec_date, gnatcov_rts_string user_data)
{
  FILE *file = fopen (filename, "wb+");
  if (!file)
    return 1;

  gnatcov_rts_generic_write_trace_file (file, buffers, program_name, exec_date,
                                        user_data, write_bytes);
  fclose (file);
  return 0;
}

/* See gnatcov_rts_c-traces-output-files.h.  */
void
gnatcov_rts_write_trace_file_wrapper
  (gnatcov_rts_unit_coverage_buffers_array *buffers,
  char *filename, gnatcov_rts_string program_name,
  uint64_t exec_date, gnatcov_rts_string user_data)
{
  if (gnatcov_rts_write_trace_file
       (buffers, filename, program_name, exec_date, user_data) != 0)
    fprintf
      (stderr,
       "Error occurred while creating the trace file %s: %s\n",
       filename, strerror (errno));
}
