/*
------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "dis_stream.h"

typedef struct disassembler_stream
{
    char *buff;
    unsigned int size;
    unsigned int idx;
} disassembler_stream;

/* Printf-like function to add content to the DS stream according to FORMAT.
   Returns the number of characters written to the stream.  */
int
stream_printf(disassembler_stream *ds, const char *format, ...)
{
  va_list ap;
  int res;

  va_start(ap, format);

  res = vsnprintf(ds->buff + ds->idx, ds->size - ds->idx, format, ap);
  ds->idx += res;

  va_end(ap);

  return res;
}

/* Empties the content of the DS stream.  */
void
clear_stream(disassembler_stream *const ds)
{
  ds->buff = NULL;
  ds->size = 0;
  ds->idx = 0;
}

/* Creates a stream.  */
disassembler_stream *
create_stream(void)
{
  disassembler_stream *ds = calloc(1, sizeof (disassembler_stream));

  return ds;
}

/* Frees memory used by the DS stream.  */
void
delete_stream(disassembler_stream *const ds)
{
  clear_stream(ds);
  free(ds);
}

/* Returns 1 if the DS stream is empty, 0 otherwise.  */
unsigned char
stream_is_empty(disassembler_stream *const ds)
{
  return !ds || !ds->size;
}

/* Sets the internal buffer of the stream.  Subsequents calls to stream_printf
   on DS will write to BUFF.  This function should be called before the first
   call to stream_printf with DS as argument.  */
void
set_stream_buffer(disassembler_stream *const ds, char *const buff, int size)
{
  ds->buff = buff;
  ds->size = size;
}
