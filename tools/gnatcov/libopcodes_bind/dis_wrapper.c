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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dis_stream.h"
#include "dis_wrapper.h"

struct disassemble_handle
{
};

/* Set necessary information for symbol resolution for the disassembler
   represented by DH.  */
void
set_disassembler_symbolizer (disassemble_handle *const dh,
                             void *const symbolizer, print_symbol_cb addr_cb)
{
  (void) dh;
  (void) symbolizer;
  (void) addr_cb;
}

/* Sets up disassembler for ARM.  */
disassemble_handle *
create_arm_disassembler (void)
{
  return malloc (1);
}

/* Sets up disassembler for Thumb.  */
disassemble_handle *
create_thumb_disassembler (void)
{
  return malloc (1);
}

/* Sets up disassembler for x86.  */
disassemble_handle *
create_x86_disassembler (void)
{
  return malloc (1);
}

disassemble_handle *
create_ppc_disassembler (void)
{
  return malloc (1);
}

disassemble_handle *
create_e500_disassembler (void)
{
  return malloc (1);
}

/* Sets up disassembler for Visium.  */
disassemble_handle *
create_visium_disassembler (void)
{
  return malloc (1);
}

disassemble_handle *
create_sparc_disassembler (void)
{
  return malloc (1);
}

disassemble_handle *
create_aarch64_disassembler (void)
{
  return malloc (1);
}

/* Frees the memory allocated for the disassembler represented by DH.  */
void
delete_disassembler (disassemble_handle *const dh)
{
  free (dh);
}

/* Disassembles binary code contained in INSN_BUFFER and copies the textual
   representation to DEST.  DEST_SIZE is the maximum capacity of DEST and
   IB_SIZE is the size of INSN_BUFFER.  PC denotes the address corresponding
   to the first instruction of INSN_BUFFER.
   Returns the size of the disassembled instruction.  */
int
disassemble_to_text (disassemble_handle *const dh, bfd_vma pc,
                     char *const dest, unsigned int dest_size,
                     bfd_byte *const insn_buffer, unsigned int ib_size,
                     enum bfd_endian endian)
{
  (void) dh;
  (void) pc;
  (void) dest;
  (void) dest_size;
  (void) insn_buffer;
  (void) ib_size;
  (void) endian;
  return 1;
}
