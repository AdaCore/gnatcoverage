/****************************************************************************
 *                                                                          *
 *                   GNATcoverage Instrumentation Runtime                   *
 *                                                                          *
 *                     Copyright (C) 2021-2022, AdaCore                     *
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

#include "gnatcov_rts_c-buffers.h"
#include "gnatcov_rts_c_strings.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

  /* Write a Base64-encoded trace file to the standard output.  See the
     documentation of the gnatcov_rts_generic_write_trace_file function in
     gnatcov_rts_c-output.h for more information.  */
  extern void gnatcov_rts_write_trace_file_base64 (
    const struct gnatcov_rts_coverage_buffers_group_array *buffers_groups,
    struct gnatcov_rts_string program_name, uint64_t exec_date,
    struct gnatcov_rts_string user_data);

#ifdef __cplusplus
}
#endif
