#ifndef DOFOR_H
# define DOFOR_H

#define GOTO_IN     (0x01 << 0)
#define GOTO_OUT    (0x01 << 1)

void
dofor (int start, int behavior);

#endif
