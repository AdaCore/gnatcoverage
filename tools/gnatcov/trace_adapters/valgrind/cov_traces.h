
/*--------------------------------------------------------------------*/
/*--- Coverage: Execution traces for GNATcoverage.    cov_traces.h ---*/
/*--------------------------------------------------------------------*/

/*
   Copyright (C) 2012, AdaCore

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


#ifndef COV_TRACES_H
#define COV_TRACES_H

#include "libvex_basictypes.h"

#define DEBUG_TRACE

/*
 * Qemu trace file format.
 * It requires proper definition for uintXX_t where XX is 8, 16, 32 and 64
 * and HWord (32 or 64 bits).
 */

typedef UChar  uint8_t;
typedef UShort uint16_t;
typedef UInt   uint32_t;
typedef ULong  uint64_t;

/* File header definition.  */
struct trace_header {
    char       magic[12];
#define QEMU_TRACE_MAGIC "#QEMU-Traces"

    uint8_t version;
#define QEMU_TRACE_VERSION 1

    /* File kind.  */
    uint8_t kind;
#define QEMU_TRACE_KIND_RAW          0
#define QEMU_TRACE_KIND_HISTORY      1
#define QEMU_TRACE_KIND_INFO         2
#define QEMU_TRACE_KIND_DECISION_MAP 3
#define QEMU_TRACE_KIND_CONSOLIDATED 248

    /* Sizeof (target_pc).  Indicates struct trace_entry length.  */
    uint8_t sizeof_target_pc;

    /* True if host was big endian.  All the trace data used the host
       endianness.  */
    uint8_t big_endian;

    /* Target machine (use ELF number) - always in big endian.  */
    uint8_t machine[2];

    uint16_t _pad;
};

struct trace_entry;

/* Header is followed by trace entries.  */

struct trace_entry32 {
    uint32_t pc;
    uint16_t size;
    uint8_t  op;
    uint8_t  _pad[1];
};

struct trace_entry64 {
    uint64_t pc;
    uint16_t size;
    uint8_t  op;
    uint8_t  _pad[5];
};

/*
 * Trace operations for RAW and HISTORY
 */

/* _BLOCK means pc .. pc+size-1 was executed.  */
#define TRACE_OP_BLOCK 0x10     /* Block fully executed.  */
#define TRACE_OP_FAULT 0x20     /* Fault at pc.  */
#define TRACE_OP_BR0   0x01     /* Branch */
#define TRACE_OP_BR1   0x02     /* Fallthrough */

/* Only used internally */
#define TRACE_OP_HIST_SET   0x100 /* Set in the map file.  */
#define TRACE_OP_HIST_CACHE 0x200 /* Has already been searched.  */

/* trace entry list */
struct trace_entry {
    HWord               pc;
    uint16_t            size;
    uint16_t             op;
    struct trace_entry *next;
    struct trace_entry *prev;
};

extern int                 tracefile_enabled;

void trace_init(const char *cov_exec_file);
void trace_cleanup(void);

/* True when full history is desired, either for all instructions or for the
 * conditional jump instruction at the end of the tb.
 */
int  tracefile_history_for_te(struct trace_entry *te);
void tracefile_history_for_te_search(struct trace_entry *te);

/* Insert a new trace_entry even if there's already one with the same pc and
 * size. This is require for branch history.
 */
struct trace_entry *new_trace_entry(HWord pc, uint16_t size);

/* Try to find a trace_entry with the same pc and size or allocate a new one if
 * needed.
 */
struct trace_entry *get_trace_entry(HWord pc, uint16_t size);

/* Takes a string containing coma separated options and returns a
 * null-terminated array of strings.
 */
char **split_trace_arg(const char *trace);

#endif /* COV_TRACES_H */
