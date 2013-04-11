#ifndef DOSWITCH_H
# define DOSWITCH_H

#define XST_ZERO            (0x1 << 0)
#define XST_ONE             (0x1 << 1)
#define XST_TWO             (0x1 << 2)
#define XST_DEFAULT         (0x1 << 3)
#define XST_INPUT_WAS_ZERO  (0x1 << 4)

void doswitch (int input, int * xstatus);

#endif
