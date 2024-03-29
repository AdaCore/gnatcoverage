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

#include "gnatcov_rts_c-traces-output-base64.h"
#include "gnatcov_rts_c-base_io.h"
#include "gnatcov_rts_c-memory.h"
#include "gnatcov_rts_c-traces-output.h"

static const char *base64_alphabet
  = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static char base64_padding = '=';

/* Various gnatcov_rts_string constants.  */
static const struct gnatcov_rts_string newline_string = STR ("\n");
static const struct gnatcov_rts_string begin_string
  = STR ("== GNATcoverage source trace file ==\n");
static const struct gnatcov_rts_string end_string = STR ("== End ==\n");

typedef struct
{
  /* We output base64 content by groups of 4 digits, which encode for 3
     input bytes. This buffer is used to accumulate input bytes.  */
  uint8_t bytes[3];

  /* Index of the next cell in Bytes where to store an input byte. This
     means that a given time, only the Bytes (1 .. Next - 1) slice contain
     input bytes.  */
  uint8_t next;

  /* Number of output digits on the current line. We keep track of this to
     break lines at 80 digits.  */
  unsigned columns;

} gnatcov_rts_base64_buffer;

static char
img (uint8_t byte)
{
  return base64_alphabet[byte];
}

/* Flush the remaining bytes in Output to the standard output. If the buffer
   is not full, this means it's the end of the content: pad with '=' bytes as
   needed.  */
static void
flush (gnatcov_rts_base64_buffer *output)
{
  uint8_t *in_bytes = output->bytes;
  char out_digits[4];
  struct gnatcov_rts_string str;

  switch (output->next)
    {
    case 0:
      return;

    case 1:
      out_digits[0] = img (in_bytes[0] >> 2);
      out_digits[1] = img ((in_bytes[0] % 4) << 4);
      out_digits[2] = base64_padding;
      out_digits[3] = base64_padding;
      break;

    case 2:
      out_digits[0] = img (in_bytes[0] >> 2);
      out_digits[1] = img (((in_bytes[0] % 4) << 4) | (in_bytes[1] >> 4));
      out_digits[2] = img ((in_bytes[1] % 16) << 2);
      out_digits[3] = base64_padding;
      break;

    default:
      /* This has to be case 3.  */
      out_digits[0] = img (in_bytes[0] >> 2);
      out_digits[1] = img (((in_bytes[0] % 4) << 4) | (in_bytes[1] >> 4));
      out_digits[2] = img (((in_bytes[1] % 16) << 2) | (in_bytes[2] >> 6));
      out_digits[3] = img (in_bytes[2] % 64);
      break;
    }

  /* Output the 4 characters corresponding to each group of 6 bits.  Introduce
     a newline when needed in order to avoid exceeding 80 characters per
     line.  */

  str.str = out_digits;
  str.length = 4;
  gnatcov_rts_put_string (str);

  output->columns += 4;
  if (output->columns >= 80)
    {
      output->columns = 0;
      gnatcov_rts_put_string (newline_string);
    }

  memset (output->bytes, 0, 4);
  output->next = 0;
}

static int
write_bytes (void *output, const void *bytes, unsigned count)
{
  gnatcov_rts_base64_buffer *buffer = (gnatcov_rts_base64_buffer *) output;
  uint8_t *bytes_array = (uint8_t *) bytes;
  unsigned i;

  for (i = 0; i < count; i++)
    {
      buffer->bytes[buffer->next] = bytes_array[i];
      buffer->next = buffer->next + 1;
      if (buffer->next == 3)
        flush (buffer);
    }
  return 0;
}

void
gnatcov_rts_write_trace_file_base64 (
  const struct gnatcov_rts_coverage_buffers_group_array *buffers_groups,
  struct gnatcov_rts_string program_name, uint64_t exec_date,
  struct gnatcov_rts_string user_data)
{
  gnatcov_rts_base64_buffer buffer;
  buffer.next = 0;
  buffer.columns = 0;
  gnatcov_rts_put_string (newline_string);
  gnatcov_rts_put_string (begin_string);
  gnatcov_rts_generic_write_trace_file (&buffer, buffers_groups, program_name,
                                        exec_date, user_data, write_bytes);
  flush (&buffer);
  if (buffer.columns != 0)
    gnatcov_rts_put_string (newline_string);
  gnatcov_rts_put_string (end_string);
}
