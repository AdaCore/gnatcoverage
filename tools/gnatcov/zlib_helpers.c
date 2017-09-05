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
