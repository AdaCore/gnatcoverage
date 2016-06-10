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

#include <string.h>

#ifdef LINUX
   /* For pid_t, used in dr_defines.h.  */
#  include <unistd.h>
#  include <sys/types.h>
#endif

#include "dr_api.h"
#include "dr_tools.h"

#include "qtrace.h"

/* History map.  This is a sorted and fixed array of PCs for which a trace
   must be written each time the instruction is executed.  */
static int    nbr_histmap_entries;
static pctype *histmap_entries;
static int    tracefile_history;

static module_data_t *main_module;
static client_id_t client_id;
static file_t tracefile;

/* Buffer of traces to be written on disk.  */
#define NBR_ENTRIES 1024
static trace_entry trace_buffer[NBR_ENTRIES];
static int nbr_entries = 0;

/* Cache for traces, to coallesce flags.  */
struct trace_cache_entry
{
  /* Address of the branch instruction.  Must be checked (for equality)
     before using the entry.  */
  app_pc addr;

  /* Branch flags.  Same as a trace entry.  */
  unsigned char op;

  /* Number of bytes in the BB before and after the instruction.  Used to
     reconstruct the trace.  */
  unsigned int blen : 20;
  unsigned int alen : 4;
};

#define NBR_CACHE_ENTRIES 102400
static struct trace_cache_entry cache_entries[NBR_CACHE_ENTRIES];
static int cache_entries_idx = 0;

static void
copy_string (char *destination, const char *source, size_t size)
{
  size_t source_size = strlen (source);

  if (source_size > size)
    source_size = size - 1;

  memcpy (destination, source, source_size);
  destination[source_size] = 0;
}

static int
tracefile_history_search (pctype pc)
{
  if (tracefile_history)
    return 1;

  if (nbr_histmap_entries)
    {
      int low  = 0;
      int high = nbr_histmap_entries - 1;

      while (low <= high)
	{
	  int   mid = low + (high - low) / 2;
	  pctype hist_pc = histmap_entries[mid];

	  if (hist_pc == pc)
	    return 1;
	  if (pc < hist_pc)
	    high = mid - 1;
	  else
	    low = mid + 1;
	}
    }
  return 0;
}

static void
flush_traces (void)
{
  ssize_t len = nbr_entries * sizeof (trace_entry);

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

static void
write_trace_cache_entry (struct trace_cache_entry *tce)
{
  write_trace
    (tce->addr - tce->blen, tce->blen + tce->alen, TRACE_OP_BLOCK | tce->op);
}

/* Clean call for the cbr */
static void
at_cbr2_nocache(app_pc inst_addr, app_pc targ_addr, app_pc fall_addr,
		int taken, void *bb_addr)
{
  (void) targ_addr;
  (void) fall_addr;
  write_trace ((app_pc)bb_addr, inst_addr + 2 - (app_pc)bb_addr,
	       TRACE_OP_BLOCK | (taken ? TRACE_OP_BR0 : TRACE_OP_BR1));
}

static void
at_cbr6_nocache(app_pc inst_addr, app_pc targ_addr, app_pc fall_addr,
		int taken, void *bb_addr)
{
  (void) targ_addr;
  (void) fall_addr;
  write_trace ((app_pc)bb_addr, inst_addr + 6 - (app_pc)bb_addr,
	       TRACE_OP_BLOCK | (taken ? TRACE_OP_BR0 : TRACE_OP_BR1));
}

static void
at_cache(app_pc inst_addr, app_pc targ_addr, app_pc fall_addr,
	 int taken, void *arg)
{
  struct trace_cache_entry *tce = (struct trace_cache_entry *)arg;

  (void) inst_addr;
  (void) targ_addr;
  (void) fall_addr;
  tce->op |= taken ? TRACE_OP_BR0 : TRACE_OP_BR1;
}

static void
flush_cb (int id)
{
  int i;

  (void) id;
  for (i = 0; i < cache_entries_idx; i++)
    write_trace_cache_entry (&cache_entries[i]);
  cache_entries_idx = 0;
}

static dr_emit_flags_t
event_basic_block(void *drcontext, void *tag, instrlist_t *bb,
                  bool for_trace, bool translating)
{
  instr_t *instr;
  app_pc bb_pc = dr_fragment_app_pc(tag);

  (void) for_trace;
  (void) translating;

  /* No instrumentation if not part of the main module.  */
  if (bb_pc < main_module->start || bb_pc > main_module->end)
    return DR_EMIT_DEFAULT;

  /* For each instruction of the basic block.  */
  for (instr = instrlist_first_app (bb); instr != NULL; )
    {
      instr_t *next_instr = instr_get_next_app(instr);

      if (instr_is_cbr(instr))
	{
	  app_pc br_pc = instr_get_app_pc (instr);
	  int br_len;

	  /* Instruction is a conditional branch.  */
	  if (next_instr != NULL)
	    {
	      /* Should be the last one of the basic block.  */
	      instrlist_disassemble(drcontext, tag, bb, STDOUT);
	      dr_exit_process (126);
	    }

	  br_len = instr_length (drcontext, instr);

	  /* Insert a call to at_cbr to always generate a trace.  */
	  if (tracefile_history_search ((pctype)br_pc))
	    {
	      void *at_fun;

	      switch (br_len)
		{
		case 2:
		  /* Conditionnal jump with a byte offset.  */
		  at_fun = (void *) at_cbr2_nocache;
		  break;
		case 6:
		  /* Conditionnal jump with a long word offset.  */
		  at_fun = (void *) at_cbr6_nocache;
		  break;
		default:
		  dr_abort ();
		}
	      dr_insert_cbr_instrumentation_ex
		(drcontext, bb, instr, at_fun, OPND_CREATE_INTPTR(bb_pc));
	    }
	  else
	    {
	      /* Allocate an entry.  */
	      struct trace_cache_entry *tce =
		&cache_entries[cache_entries_idx++];

	      /* Near end of cache.  */
	      if (cache_entries_idx >= NBR_CACHE_ENTRIES - 16)
		{
		  /* Request for a flush.  */
		  if (cache_entries_idx == NBR_CACHE_ENTRIES - 16)
		    {
		      if (!dr_delay_flush_region (0, (size_t)-1, 0, flush_cb))
			dr_abort ();
		    }
		  else if (cache_entries_idx == NBR_CACHE_ENTRIES)
		    {
		      /* No more entries -> abort.  */
		      dr_abort ();
		    }
		}

	      /* Initialize it.  */
	      tce->addr = br_pc;
	      tce->op = 0;
	      tce->blen = br_pc - bb_pc;
	      tce->alen = br_len;

	      dr_insert_cbr_instrumentation_ex
		(drcontext, bb, instr, at_cache, OPND_CREATE_INTPTR(tce));
	    }
        }
        else if (instr_is_call_direct(instr)
		 || instr_is_call_indirect(instr)
		 || instr_is_return(instr)
		 || instr_is_ubr(instr)
		 || instr_is_mbr(instr))
	  {
	    /* This is an unconditional branch.  */
	    app_pc next_pc = instr_get_app_pc (instr)
	      + instr_length (drcontext, instr);
	    if (next_instr != NULL)
	      {
		/* Must be the last instruction of the basic block.  */
		instrlist_disassemble(drcontext, tag, bb, STDOUT);
		dr_exit_process (126);
	      }
	    /* Generate the trace now, assuming the basic block will be
	       fully executed.  */
	    write_trace (bb_pc, next_pc - bb_pc, TRACE_OP_BLOCK);
	}
	instr = next_instr;
    }
    return DR_EMIT_DEFAULT;
}

#ifdef LINUX
/* Disable all client features/effects for child processes.  */
static void
event_fork_init (void *drcontext)
{
  (void) drcontext;

  /* Invalidate all translated blocks so that all instrumentation is gone.  */
  if (!dr_delay_flush_region (0, (size_t)-1, 0, NULL))
    dr_abort ();

  dr_unregister_bb_event (event_basic_block);
}
#endif

static void
event_exit(void)
{
  int i;

#if 0
  dr_fprintf (STDERR, "flushing caches\n");
  write_trace (0, 0, 0);
#endif

  for (i = 0; i < cache_entries_idx; i++)
    write_trace_cache_entry (&cache_entries[i]);
  flush_traces ();
  dr_close_file (tracefile);
  tracefile = INVALID_FILE;
}

static void
read_map_file(const char *filename)
{
  file_t histfile;
  struct trace_header hdr;
  uint64 length;
  int ent_sz;
  int i;

  histfile = dr_open_file (filename, DR_FILE_READ);

  if (tracefile == INVALID_FILE)
    {
      dr_fprintf (STDERR, "cannot open file %s\n", filename);
      dr_exit_process (127);
    }

  if (dr_read_file (histfile, &hdr, sizeof(hdr)) != sizeof(hdr))
    {
      dr_fprintf (STDERR,
		  "cannot read trace header for histmap file '%s'\n",
		  filename);
      dr_exit_process (127);
    }

  if (memcmp (hdr.magic, QEMU_TRACE_MAGIC, sizeof(hdr.magic)) != 0
      || hdr.version != QEMU_TRACE_VERSION
      || hdr.kind != QEMU_TRACE_KIND_DECISION_MAP
      || hdr.sizeof_target_pc != sizeof (void *)
      || (hdr.big_endian != 0 && hdr.big_endian != 1)
      || hdr.machine[0] != (ELF_MACHINE >> 8)
      || hdr.machine[1] != (ELF_MACHINE & 0xff)
      || hdr._pad != 0)
    {
      dr_fprintf (STDERR, "bad header for histmap file '%s'\n", filename);
      dr_exit_process (127);
    }

    /* Get number of entries. */
  if (!dr_file_size (histfile, &length))
    {
      dr_fprintf (STDERR, "cannot get size of histmap file '%s'\n", filename);
      dr_exit_process (127);
    }

  if (!dr_file_seek (histfile, sizeof(hdr), DR_SEEK_SET))
    {
      dr_fprintf (STDERR, "cannot set seek of histmap file '%s'\n", filename);
      dr_exit_process (127);
    }

  length -= sizeof(hdr);
  if (sizeof (void *) == 4)
    ent_sz = sizeof(struct trace_entry32);
  else
    ent_sz = sizeof(struct trace_entry64);

  if ((length % ent_sz) != 0)
    {
      dr_fprintf (STDERR, "bad length of histmap file '%s'\n", filename);
      dr_exit_process (127);
    }

  nbr_histmap_entries = (int)(length / ent_sz);
  if (nbr_histmap_entries)
    histmap_entries = dr_global_alloc (nbr_histmap_entries * sizeof(void *));

  for (i = 0; i < nbr_histmap_entries; i++)
    {
      trace_entry ent;

      if (dr_read_file (histfile, &ent, sizeof(ent)) != sizeof(ent))
	{
	  dr_fprintf (STDERR, "cannot read histmap entry from '%s'\n",
		      filename);
	  dr_exit_process (127);
	}
      if (i > 0 && ent.pc < histmap_entries[i - 1])
	{
	  dr_fprintf (STDERR, "unordered entry #%d in histmap file '%s'\n",
		      i, filename);
	  dr_exit_process (127);
	}

      histmap_entries[i] = ent.pc;
    }
  dr_close_file (histfile);
}

static void
create_trace_file (const char *filename)
{
  struct trace_header hdr;

  tracefile = dr_open_file (filename,
			     DR_FILE_CLOSE_ON_FORK | DR_FILE_WRITE_APPEND);

  if (tracefile == INVALID_FILE)
    {
      dr_fprintf (STDERR, "cannot open file %s\n", filename);
      dr_exit_process (127);
    }

  memcpy (hdr.magic, QEMU_TRACE_MAGIC, strlen (QEMU_TRACE_MAGIC));
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
void dr_client_main(client_id_t id, int argc, const char *argv[])
{
  char filename[MAXIMUM_PATH] = "dynamorio.trace";
  char histmap[MAXIMUM_PATH] = "";

#ifdef WINDOWS
  dr_enable_console_printing();
#endif /* WINDOWS */

  dr_set_client_name("DynamoRIO Sample Client 'qtrace'",
		     "http://dynamorio.org/issues");
  dr_log(NULL, LOG_ALL, 1, "Client 'qtrace' initializing");
  client_id = id;

  /* Decode options.  */
  for (int i = 1; i < argc; ++i)
    {
      if (strcmp (argv[i], "-o") == 0)
	{
	  i++;
	  if (i >= argc)
	    {
	      dr_fprintf (STDERR, "error: missing trace output filename\n");
	      dr_exit_process (1);
	    }

	  /* Strip 'history,' and 'histmap='.  */
	  const char *arg = argv[i];

	  if (memcmp (arg, "history,", 8) == 0)
	    arg = arg + 8;

	  if (memcmp (arg, "histmap=", 8) == 0)
	    {
	      const char *histmap_start = arg + 8;
	      const char *histmap_end = histmap_start;

	      while (*histmap_end && *histmap_end != ',')
		++histmap_end;
	      const unsigned histmap_len = histmap_end - histmap_start;

	      memcpy (histmap, histmap_start, histmap_len);
	      histmap[histmap_len] = 0;

	      if (*histmap_end == 0)
		{
		  dr_fprintf (STDERR,
			      "error: missing trace output filename\n");
		  dr_exit_process (1);
		}
	      arg = histmap_end + 1;
	    }
	  else
	    histmap[0] = 0;

	  copy_string (filename, arg, sizeof (filename));
	}
      else
	{
	  dr_fprintf (STDERR, "error: invalid option: %s\n", argv[i]);
	  dr_exit_process (1);
	}
    }

  /* Intercept all BB translation.  */
  dr_register_bb_event(event_basic_block);

#ifdef LINUX
  dr_register_fork_init_event(event_fork_init);
#endif
  dr_register_exit_event(event_exit);

  if (histmap[0])
    read_map_file (histmap);

  create_trace_file (filename);

  main_module = dr_get_main_module ();
  DR_ASSERT_MSG (main_module != NULL, "cannot get main_module");
}
