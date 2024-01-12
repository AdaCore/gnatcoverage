/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2024, AdaCore                     *
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

/* This header provides the smallest subset of functions needed to output
   bytes.  These functions are used in gnatcov_rts_c-traces-output-base64.c
   (and thus by the implementation of gnatcov's --dump-channel=base64-stdout
   option) to dump base64 traces to "the output": stdout when linking with a C
   runtime only, and GNAT.IO.Standard_Output when linking with an Ada runtime.

   In the former case, the gnatcov_rts_c-base_io.c module implements these
   functions, and in the latter case, it’s the GNATcov_RTS.Base_IO package that
   implements them (these two units conflict with each other, so we include
   only one of them at link time).  */

#include "gnatcov_rts_c-strings.h"

#ifdef __cplusplus
extern "C"
{
#endif

  /* See gnatcov_rts-base_io.ads.  */
  extern int gnatcov_rts_put_string (struct gnatcov_rts_string str);

#ifdef __cplusplus
}
#endif
