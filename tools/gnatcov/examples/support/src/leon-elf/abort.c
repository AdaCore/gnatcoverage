/****************************************************************************
 *                                                                          *
 *                              GNATcoverage                                *
 *                                                                          *
 *                      Copyright (C) 2012, AdaCore                         *
 *                                                                          *
 * GNATcoverage is free software; you can redistribute it  and/or modify it *
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

/* Our local last chance handlers for Ada rely on "abort", and this is not
   part of the standard RTS on leon.  */

void abort (void)
{
  asm ("mov 0, %g1; ta 0");
}
