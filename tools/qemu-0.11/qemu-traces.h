/*
 * QEMU System Emulator
 *
 * Copyright (C) 2009, AdaCore
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

/*
 * Qemu trace file format.
 * It requires proper definition for uintXX_t where XX is 8, 16, 32 and 64
 * and target_ulong (32 or 64 bits).
 */

#ifndef QEMU_TRACE_H
#define QEMU_TRACE_H

/* File header definition.  */
struct trace_header
{
    char magic[12];
#define QEMU_TRACE_MAGIC "#QEMU-Traces"

    uint8_t version;
#define QEMU_TRACE_VERSION 1

    /* File kind.  */
    uint8_t kind;
#define QEMU_TRACE_KIND_RAW            0
#define QEMU_TRACE_KIND_HISTORY        1
#define QEMU_TRACE_KIND_INFO           2
#define QEMU_TRACE_KIND_DECISION_MAP   3
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

/* Header is followed by trace entries.  */
struct trace_entry
{
    target_ulong pc;
    uint16_t size;
    uint8_t op;
};

struct trace_entry32
{
    uint32_t pc;
    uint16_t size;
    uint8_t op;
    uint8_t _pad[1];
};

struct trace_entry64
{
    uint64_t pc;
    uint16_t size;
    uint8_t op;
    uint8_t _pad[5];
};

/*
 * Trace operations for RAW and HISTORY
 */

/* _BLOCK means pc .. pc+size-1 was executed.  */
#define TRACE_OP_BLOCK 0x10 /* Block fully executed.  */
#define TRACE_OP_FAULT 0x20 /* Fault at pc.  */
#define TRACE_OP_BR0 0x01 /* Branch 0 taken at pc.  */
#define TRACE_OP_BR1 0x02

/* Only used internally in cpu-exec.c.  */
#define TRACE_OP_HIST_SET 0x100		/* Set in the map file.  */
#define TRACE_OP_HIST_CACHE 0x200	/* Has already been searched.  */

/*
 * Decision map operations
 */

/* Trace conditional jump instruction at address */
#define TRACE_OP_TRACE_CONDITIONAL 1

extern struct trace_entry *trace_current;
extern int tracefile_enabled;
extern int tracefile_nobuf;
extern int tracefile_history;

void trace_init (const char *optarg);
void trace_push_entry (void);

#endif /* QEMU_TRACE_H */
