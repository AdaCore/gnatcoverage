/*
------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
*/

#ifndef DIS_WRAPPER_H_
#define DIS_WRAPPER_H_

/* The following defines are needed to bypass check in libbfd include.  */
#define PACKAGE 1
#define PACKAGE_VERSION 1

#include <bfd.h>
#include <dis-asm.h>

typedef struct disassemble_handle disassemble_handle;

/* Functions of this type can write at most BUFF_SIZE bytes.  */
typedef int (*print_symbol_cb) (bfd_vma addr, void *symbolizer,
                                char *const buff, int buff_size);

extern disassemble_handle *create_arm_disassembler (void);

extern disassemble_handle *create_thumb_disassembler (void);

extern disassemble_handle *create_x86_disassembler (void);

extern disassemble_handle *create_visium_disassembler (void);

extern void delete_disassembler (disassemble_handle *const dh);

extern int disassemble_to_text (disassemble_handle *const dh, bfd_vma pc,
                                char *const dest, unsigned int dest_size,
                                bfd_byte *const insn_buffer,
                                unsigned int ib_size, enum bfd_endian endian);

extern void set_disassembler_symbolizer (disassemble_handle *const dh,
                                         void *const symbolizer,
                                         print_symbol_cb addr_cb);

#endif /* !DIS_WRAPPER_H_ */
