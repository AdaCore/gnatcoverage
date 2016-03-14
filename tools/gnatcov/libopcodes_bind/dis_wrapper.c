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

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "dis_wrapper.h"
#include "dis_stream.h"

/* Define the number of values in enum bfd_endian.  */
#define N_BFD_ENDIAN_VALUES 3

typedef struct disassemble_handle
{
    disassembler_ftype disass_func[N_BFD_ENDIAN_VALUES];
    disassemble_info  dinfo;
} disassemble_handle;

/* Type stored in dhandle->dinfo->application_data.  */
typedef struct symbolizer_data
{
    print_symbol_cb addr_cb; /* Function that performs the symbol lookup.  */
    void *symbolizer; /* Object containing the symbol informations.  */
} symbolizer_data;

/* This is a callback for disassemble_info->print_address_func.  */
static void
_print_address_cb (bfd_vma addr, disassemble_info *dinfo)
{
  symbolizer_data *sym_data = dinfo->application_data;
  char buff[128];
  int printed = sym_data->addr_cb (addr, sym_data->symbolizer, buff, 127);

  if (printed > 0)
    {
      buff[printed] = '\0';
      stream_printf (dinfo->stream, "%s", buff);
    }
  else
    /* No symbol found at addr.  */
#if TARGET_BITS == 32
    stream_printf (dinfo->stream, "0x%08lx", addr);
#elif TARGET_BITS == 64
    stream_printf (dinfo->stream, "0x%016lx", addr);
#else /* TARGET_BITS != 32 and TARGET_BITS != 64 */
#error "Target arch is neither 32 or 64bits, not supported."
#endif
}


/* Create disassembler set up for ARM.
   If FOR_THUMB is true, the disassembler will disassemble Thumb instruction
   set.  Otherwise the disassembler will disassemble ARM instruction set.  */
static disassemble_handle *
_create_arm_arch_disassembler (unsigned char for_thumb)
{
  disassemble_handle *dh = malloc (sizeof (disassemble_handle));

  if (!dh)
    return NULL;

  {
    disassembler_stream *ds = create_stream ();

    if (!ds)
      {
        free (dh);
        return NULL;
      }

    init_disassemble_info (&(dh->dinfo), ds, (fprintf_ftype) stream_printf);

    dh->dinfo.arch = bfd_arch_arm;

    dh->dinfo.application_data = calloc (1, sizeof (symbolizer_data));

    if (for_thumb)
      dh->dinfo.disassembler_options = strdup ("force-thumb");

    disassemble_init_for_target (&(dh->dinfo));

    dh->disass_func[BFD_ENDIAN_BIG] = print_insn_big_arm;
    dh->disass_func[BFD_ENDIAN_LITTLE] = print_insn_little_arm;
    dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;
  }

  return dh;
}

/* Set necessary information for symbol resolution for the disassembler
   represented by DH.  */
void
set_disassembler_symbolizer (disassemble_handle *const dh,
                             void *const symbolizer, print_symbol_cb addr_cb)
{
  symbolizer_data* sym_data = dh->dinfo.application_data;

  sym_data->addr_cb = addr_cb;
  dh->dinfo.print_address_func = _print_address_cb;
  sym_data->symbolizer = symbolizer;
}

/* Sets up disassembler for ARM.  */
disassemble_handle *
create_arm_disassembler (void)
{
  return _create_arm_arch_disassembler (0);
}

/* Sets up disassembler for Thumb.  */
disassemble_handle *
create_thumb_disassembler (void)
{
  return _create_arm_arch_disassembler (1);
}

/* Frees the memory allocated for the disassembler represented by DH.  */
void
delete_disassembler (disassemble_handle *const dh)
{
  delete_stream (dh->dinfo.stream);

  free (dh->dinfo.application_data);
  dh->dinfo.application_data = NULL;

  free (dh->dinfo.disassembler_options);
  dh->dinfo.disassembler_options = NULL;

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
  int size;

  /* Stream should be empty at this point.  */
  assert (stream_is_empty (dh->dinfo.stream));

  dh->dinfo.buffer = insn_buffer;
  dh->dinfo.buffer_vma = pc;
  dh->dinfo.buffer_length = ib_size;

  dh->dinfo.endian = endian;
  dh->dinfo.endian_code = endian;

  set_stream_buffer (dh->dinfo.stream, dest, dest_size);

  size = dh->disass_func[endian] (pc, &(dh->dinfo));

  clear_stream (dh->dinfo.stream);

  return size;
}
