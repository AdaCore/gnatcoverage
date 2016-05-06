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
    /* Libopcode sets dinfo.disassembler_options to NULL after parsing once.
       This is erroneous when we want e.g. to switch between ARM and Thumb
       multiple times.  So we need to keep our disassembler options.  */
    char *disassembler_options;
} disassemble_handle;

/* Type stored in dhandle->dinfo->application_data.  */
typedef struct symbolizer_data
{
    print_symbol_cb addr_cb; /* Function that performs the symbol lookup.  */
    void *symbolizer; /* Object containing the symbol informations.  */
} symbolizer_data;

char *const dis_thumb_option = "force-thumb";
char *const dis_arm_option = "no-force-thumb";
char *const dis_x86_option = "i386";
char *const dis_x86_64_option = "x86-64";
char *const dis_ppc_32_option = "32";
char *const dis_ppc_64_option = "64";

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

/* Allocates and sets up data common to all disassemblers.  */
static disassemble_handle *
_create_base_disassembler (enum bfd_architecture arch)
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

    dh->dinfo.arch = arch;

    disassemble_init_for_target (&(dh->dinfo));

    dh->dinfo.application_data = calloc (1, sizeof (symbolizer_data));
  }

  return dh;
}

/* Create disassembler set up for ARM.
   If FOR_THUMB is true, the disassembler will disassemble Thumb instruction
   set.  Otherwise the disassembler will disassemble ARM instruction set.  */
static disassemble_handle *
_create_arm_arch_disassembler (unsigned char for_thumb)
{
  disassemble_handle *dh = _create_base_disassembler (bfd_arch_arm);

  if (!dh)
    return NULL;

  if (for_thumb)
    dh->disassembler_options = dis_thumb_option;
  else
    dh->disassembler_options = dis_arm_option;

  dh->disass_func[BFD_ENDIAN_BIG] = print_insn_big_arm;
  dh->disass_func[BFD_ENDIAN_LITTLE] = print_insn_little_arm;
  dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;

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

/* Sets up disassembler for x86.  */
disassemble_handle *
create_x86_disassembler (void)
{
  disassemble_handle *dh = _create_base_disassembler (bfd_arch_i386);

  if (!dh)
    return NULL;

#if TARGET_BITS == 32
  dh->disassembler_options = dis_x86_option;
#elif TARGET_BITS == 64
  dh->disassembler_options = dis_x86_64_option;
#else /* TARGET_BITS != 32 and TARGET_BITS != 64 */
#error "Target arch is neither 32 or 64bits, not supported."
#endif

  dh->disass_func[BFD_ENDIAN_BIG] = NULL;
  dh->disass_func[BFD_ENDIAN_LITTLE] = print_insn_i386;
  dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;

  return dh;
}

disassemble_handle *
create_ppc_disassembler (void)
{
  disassemble_handle *dh = _create_base_disassembler (bfd_arch_powerpc);

  if (!dh)
    return NULL;

#if TARGET_BITS == 32
  dh->disassembler_options = dis_ppc_32_option;
#elif TARGET_BITS == 64
  dh->disassembler_options = dis_ppc_64_option;
#else /* TARGET_BITS != 32 and TARGET_BITS != 64 */
#error "Target arch is neither 32 or 64bits, not supported."
#endif

  dh->disass_func[BFD_ENDIAN_BIG] = print_insn_big_powerpc;
  dh->disass_func[BFD_ENDIAN_LITTLE] = print_insn_little_powerpc;
  dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;

  return dh;
}

/* Sets up disassembler for Visium.  */
disassemble_handle *
create_visium_disassembler (void)
{
  disassemble_handle *dh = _create_base_disassembler (bfd_arch_visium);

  if (!dh)
    return NULL;

  dh->disass_func[BFD_ENDIAN_BIG] = print_insn_visium;
  dh->disass_func[BFD_ENDIAN_LITTLE] = NULL;
  dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;

  return dh;
}

disassemble_handle *
create_sparc_disassembler (void)
{
  disassemble_handle *dh = _create_base_disassembler (bfd_arch_sparc);

  if (!dh)
    return NULL;

  /* print_insn_sparc handles both big and little endian.  */
  dh->disass_func[BFD_ENDIAN_BIG] = print_insn_sparc;
  dh->disass_func[BFD_ENDIAN_LITTLE] = print_insn_sparc;
  dh->disass_func[BFD_ENDIAN_UNKNOWN] = NULL;

  return dh;
}

/* Frees the memory allocated for the disassembler represented by DH.  */
void
delete_disassembler (disassemble_handle *const dh)
{
  delete_stream (dh->dinfo.stream);

  free (dh->dinfo.application_data);
  dh->dinfo.application_data = NULL;

  dh->disassembler_options = NULL;

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

  /* Make sure disassembler options are maintained to disassemble each
     instruction.  */
  dh->dinfo.disassembler_options = dh->disassembler_options;

  size = dh->disass_func[endian] (pc, &(dh->dinfo));

  clear_stream (dh->dinfo.stream);

  return size;
}
