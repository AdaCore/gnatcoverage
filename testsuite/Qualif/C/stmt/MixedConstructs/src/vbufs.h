#ifndef VBUFS_H
# define VBUFS_H

# define HIST_SIZE 10

struct vbuffer
{
  int store[HIST_SIZE]; /* Array of entries.  */
  unsigned size;        /* Number of entry slots in the array.  */
  unsigned len;         /* Number of filled entries.  */
};

extern void
vbuffer_init (struct vbuffer *vbuf);

extern void
vbuffer_push (int value, struct vbuffer *vbuf);

#endif
