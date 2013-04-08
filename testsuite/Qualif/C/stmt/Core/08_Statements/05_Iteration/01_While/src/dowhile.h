#ifndef DOWHILE_H
# define DOWHILE_H

# define GOTO_IN        (0x01 << 0)
# define GOTO_OUT       (0x01 << 1)

void dowhile (int start, int behavior);

#endif
