#include "vbufs.h"

void
vbuffer_init (struct vbuffer *vbuf)
{
  vbuf->size = HIST_SIZE;
  vbuf->len = 0;
}

void
vbuffer_push (int value, struct vbuffer *vbuf)
{
  int i;

  /* Shift right, insert at first position the given VALUE and adjust
     length.  */
  for (i = vbuf->size - 1; i > 0; --i)          // # PU_loop0
    {
      if (i < vbuf->size - 1)                   // # PU_tshift
        vbuf->store[i] = vbuf->store[i - 1];    // # PU_shift
    }
  vbuf->store[0] = value;                       // # PU_update
  vbuf->len = (vbuf->len < vbuf->size) ? (vbuf->len + 1) : vbuf->len; // # PU_update
}
