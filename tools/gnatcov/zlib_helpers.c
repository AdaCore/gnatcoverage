/****************************************************************************
 *                                                                          *
 *                               GNATcoverage                               *
 *                                                                          *
 *                     Copyright (C) 2017-2022, AdaCore                     *
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
#include <string.h>

#include <zlib.h>

/* Uncompress the content of IN_BUFFER (a byte buffer of size IN_SIZE) into
   OUT_BUFFER (a byte buffer of size OUT_SIZE) using zlib.  Return whether
   decompression was successful.  */
int
gnatcov_zlib_uncompress (char *in_buffer, uint64_t in_size,
			 char *out_buffer, uint64_t out_size)
{
  z_stream strm;

  memset (&strm, 0, sizeof (strm));
  strm.avail_in = in_size;
  strm.next_in = (Bytef *) in_buffer;
  strm.avail_out = out_size;
  strm.next_out = (Bytef *) out_buffer;

  /* Uncompress and then check that uncompressed data consumed exactly OUT_SIZE
     bytes.  */
  return (inflateInit (&strm) == Z_OK
	  && inflate (&strm, Z_FINISH) == Z_OK
	  && inflateEnd (&strm) == Z_OK
	  && strm.avail_out == 0);
}
