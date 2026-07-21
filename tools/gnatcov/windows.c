/****************************************************************************
 *                                                                          *
 *                               GNATcoverage                               *
 *                                                                          *
 *                        Copyright (C) 2026, AdaCore                       *
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

/* This unit provides C wrappers for Windows APIs, so they can be used from
   Ada code.  They act as stubs on other systems, so that Ada code does not
   need conditional compilation.  */

#ifdef _WIN32
#include "sysinfoapi.h"
#endif

int
gnatcov_get_system_directory (char *result, int size)
{
#ifdef _WIN32
  return GetSystemDirectory (result, size);
#else
  (void) result;
  (void) size;
  return 0;
#endif
}

int
gnatcov_get_windows_directory (char *result, int size)
{
#ifdef _WIN32
  return GetWindowsDirectory (result, size);
#else
  (void) result;
  (void) size;
  return 0;
#endif
}
