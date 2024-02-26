/****************************************************************************
 *                                                                          *
 *                               GNATcoverage                               *
 *                                                                          *
 *                     Copyright (C) 2008-2024, AdaCore                     *
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

#ifndef DIS_STREAM_H_
#define DIS_STREAM_H_

/* The following defines are needed to bypass check in libbfd include.  */
#define PACKAGE 1
#define PACKAGE_VERSION 1

#include <bfd.h>
#include <dis-asm.h>

struct disassembler_stream;
typedef struct disassembler_stream disassembler_stream;

extern disassembler_stream *create_stream (void);

extern void delete_stream (disassembler_stream *const ds);

extern int stream_printf (disassembler_stream *ptr, const char *format, ...);

extern int stream_styled_printf (disassembler_stream *ptr,
                                 enum disassembler_style style,
                                 const char *format, ...);

extern void clear_stream (disassembler_stream *const ds);

extern unsigned char stream_is_empty (disassembler_stream *const ds);

extern void set_stream_buffer (disassembler_stream *const ds, char *const buff,
                               int size);

#endif /* !DIS_STREAM_H_ */
