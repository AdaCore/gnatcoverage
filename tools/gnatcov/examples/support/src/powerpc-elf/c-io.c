/****************************************************************************
 *                                                                          *
 *                              Couverture                                  *
 *                                                                          *
 *                     Copyright (C) 2008-2010, AdaCore                     *
 *                                                                          *
 * Couverture is free software; you can redistribute it  and/or modify it   *
 * under terms of the GNU General Public License as published by the Free   *
 * Software Foundation; either version 2, or (at your option) any later     *
 * version.  Couverture is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License  for more details. You  should  have  received a copy of the GNU *
 * General Public License  distributed with GNAT; see file COPYING. If not, *
 * write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, *
 * Boston, MA 02111-1307, USA.                                              *
 *                                                                          *
 ****************************************************************************/

#include <board-io.h>

/* serial registers */

#ifdef PPC_PREP
#define SERIAL0 0x3f8
#elif defined PPC_SBC834x
#define SERIAL0 0x4500
#endif

#define RBR 0
#define THR 0
#define IER 1
#define IIR 2
#define LCR 3
#define MCR 4
#define LSR 5
#define MSR 6
#define SCR 7

int putchar(int c)
{
  __outb (SERIAL0 + THR, c);
  return c;
}

static int
__checkkey (void)
{
  return __inb (SERIAL0 + LSR) & 0x01;
}

int getchar (void)
{
  while (!__checkkey ())
    ;
  return __inb(SERIAL0 + RBR);
}

int fsync (int fd)
{
}
