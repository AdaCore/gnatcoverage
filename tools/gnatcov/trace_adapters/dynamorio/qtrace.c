/*
   Copyright (C) 2013, AdaCore

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "dr_api.h"

#define EM_386         3        /* Intel 80386 */
#define EM_X86_64      62       /* AMD x86-64 architecture */

/* File header definition.  */
struct trace_header {
  char       magic[12];
#define QEMU_TRACE_MAGIC "#QEMU-Traces"

  unsigned char version;
#define QEMU_TRACE_VERSION 1

  /* File kind.  */
  unsigned char kind;
#define QEMU_TRACE_KIND_RAW          0
#define QEMU_TRACE_KIND_HISTORY      1
#define QEMU_TRACE_KIND_INFO         2
#define QEMU_TRACE_KIND_DECISION_MAP 3
#define QEMU_TRACE_KIND_CONSOLIDATED 248

  /* Sizeof (target_pc).  Indicates struct trace_entry length.  */
  unsigned char sizeof_target_pc;

  /* True if host was big endian.  All the trace data used the host
     endianness.  */
  unsigned char big_endian;

    /* Target machine (use ELF number) - always in big endian.  */
  unsigned char machine[2];

  unsigned short _pad;
};

/* Header is followed by trace entries.  */

struct trace_entry32 {
    unsigned long pc;
    unsigned short size;
    unsigned char op;
    unsigned char  _pad[1];
};

struct trace_entry64 {
  unsigned long long pc;
  unsigned short size;
  unsigned char  op;
  unsigned char  _pad[5];
};

/* _BLOCK means pc .. pc+size-1 was executed.  */
#define TRACE_OP_BLOCK 0x10     /* Block fully executed.  */
#define TRACE_OP_FAULT 0x20     /* Fault at pc.  */
#define TRACE_OP_BR0   0x01     /* Branch */
#define TRACE_OP_BR1   0x02     /* Fallthrough */

#ifdef X86_32
#  define ELF_MACHINE      EM_386
typedef struct trace_entry32 trace_entry;
#elif defined(X86_64)
#  define ELF_MACHINE      EM_X86_64
typedef struct trace_entry64 trace_entry;
#else
#  error "unhandled machine"
#endif

static client_id_t client_id;
static file_t tracefile;

#define NBR_ENTRIES 1024
static trace_entry trace_buffer[NBR_ENTRIES];
static int nbr_entries = 0;

static void
flush_traces (void)
{
  size_t len = nbr_entries * sizeof (trace_entry);

  if (nbr_entries == 0)
    return;
  if (dr_write_file (tracefile, trace_buffer, len) != len)
    {
      dr_fprintf (STDERR, "cannot write trace\n");
      dr_exit_process (127);
    }
  nbr_entries = 0;
}

static void
write_trace (app_pc pc, unsigned int size, unsigned char op)
{
  trace_entry *ent;

  if (nbr_entries == NBR_ENTRIES)
    flush_traces ();

  /* FIXME: atomic access ?  */
  ent = &trace_buffer[nbr_entries++];
  ent->pc = (unsigned long) pc;
  ent->size = size;
  ent->op = op;
}

/* Clean call for the cbr */
static void
at_cbr(app_pc inst_addr, app_pc targ_addr, app_pc fall_addr, int taken,
       void *bb_addr)
{
  write_trace ((app_pc)bb_addr, inst_addr - (app_pc)bb_addr + 1,
	       TRACE_OP_BLOCK | (taken ? TRACE_OP_BR0 : TRACE_OP_BR1));
}

static dr_emit_flags_t
event_basic_block(void *drcontext, void *tag, instrlist_t *bb,
                  bool for_trace, bool translating)
{
  instr_t *instr;
  app_pc bb_pc = dr_fragment_app_pc(tag);

  for (instr  = instrlist_first_app(bb); instr != NULL; )
    {
      instr_t *next_instr = instr_get_next_app(instr);
      if (instr_is_cbr(instr))
	{
	  if (next_instr != NULL)
	    {
	      instrlist_disassemble(drcontext, tag, bb, STDOUT);
	      dr_exit_process (126);
	    }
	  dr_insert_cbr_instrumentation_ex
	    (drcontext, bb, instr, (void *)at_cbr, OPND_CREATE_INTPTR(bb_pc));
        }
        else if (instr_is_call_direct(instr)
		 || instr_is_call_indirect(instr)
		 || instr_is_return(instr)
		 || instr_is_ubr(instr)
		 || instr_is_mbr(instr))
	  {
	    app_pc instr_pc = instr_get_app_pc (instr);
	    if (next_instr != NULL)
	      {
		instrlist_disassemble(drcontext, tag, bb, STDOUT);
		dr_exit_process (126);
	      }
	    write_trace (bb_pc, instr_pc - bb_pc + 1, TRACE_OP_BLOCK);
	}
	instr = next_instr;
    }
    return DR_EMIT_DEFAULT;
}

static void
event_exit(void)
{
  flush_traces ();
  dr_close_file (tracefile);
  tracefile = INVALID_FILE;
}

static void
create_trace_file (const char *filename)
{
  struct trace_header hdr = { QEMU_TRACE_MAGIC };

  tracefile = dr_open_file (filename,
			     DR_FILE_CLOSE_ON_FORK | DR_FILE_WRITE_APPEND);

  if (tracefile == INVALID_FILE)
    {
      dr_fprintf (STDERR, "cannot open file %s\n", filename);
      dr_exit_process (127);
    }

  hdr.version = QEMU_TRACE_VERSION;
  hdr.sizeof_target_pc = sizeof (void *);
  hdr.kind = QEMU_TRACE_KIND_RAW;
  hdr.big_endian = 0; /* x86 is little endian.  */
  hdr.machine[0] = ELF_MACHINE >> 8;
  hdr.machine[1] = ELF_MACHINE & 0xff;

  if (dr_write_file (tracefile, &hdr, sizeof(hdr)) != sizeof(hdr))
    {
      dr_fprintf (STDERR, "cannot write trace header\n");
      dr_exit_process (127);
  }
}

DR_EXPORT
void dr_init(client_id_t id)
{
  char filename[MAXIMUM_PATH];
  char arg[MAXIMUM_PATH + 8];
  const char *p;

  dr_set_client_name("DynamoRIO Sample Client 'cbrtrace'",
		     "http://dynamorio.org/issues");
  dr_log(NULL, LOG_ALL, 1, "Client 'cbrtrace' initializing");
  client_id = id;

  strcpy_s (filename, sizeof (filename), "dynamorio.trace");

  /* Decode options.  */
  p = dr_get_options (id);
  while ((p = dr_get_token (p, arg, sizeof (arg))) != NULL)
    {
      if (strcmp (arg, "-o") == 0)
	{
	  p = dr_get_token (p, filename, sizeof (filename));
	  DR_ASSERT_MSG (p != NULL, "missing -o filename");
	}
      else
	DR_ASSERT_MSG (false, "invalid option");
    }

  dr_register_bb_event(event_basic_block);
  dr_register_exit_event(event_exit);

#ifdef WINDOWS
  dr_enable_console_printing();
#endif /* WINDOWS */

  create_trace_file (filename);
}
