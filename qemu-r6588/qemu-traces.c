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

#include <stdio.h>
#include <stdlib.h>
#include "qemu-common.h"
#include "config.h"
#include "qemu-traces.h"
#include "elf.h"

static FILE *tracefile;

#define MAX_TRACE_ENTRIES 1024
static struct trace_entry trace_entries[MAX_TRACE_ENTRIES];

struct trace_entry *trace_current = trace_entries;
int tracefile_enabled = 0;
int tracefile_nobuf = 0;
int tracefile_history = 0;

static void
trace_flush(void)
{
    size_t len = (trace_current - trace_entries) * sizeof (trace_entries[0]);
    fwrite(trace_entries, len, 1, tracefile);
    trace_current = trace_entries;
    if (tracefile_nobuf)
      fflush(tracefile);
}

static void trace_cleanup (void)
{
    trace_flush ();
    fclose(tracefile);
}

int
trace_init (const char *tracefile_name, int noappend)
{
    static struct trace_header hdr = { QEMU_TRACE_MAGIC };
    const char *mode = noappend ? "wb" : "ab";

    tracefile = fopen(tracefile_name, mode);
    if (tracefile == NULL)
	return -1;

    hdr.version = QEMU_TRACE_VERSION;
    hdr.sizeof_target_pc = sizeof(target_ulong);
    hdr.kind = tracefile_history ?
	QEMU_TRACE_KIND_HISTORY : QEMU_TRACE_KIND_RAW;
#ifdef WORDS_BIGENDIAN
    hdr.big_endian = 1;
#else
    hdr.big_endian = 0;
#endif
    hdr.machine[0] = ELF_MACHINE >> 8;
    hdr.machine[1] = ELF_MACHINE;
    fwrite(&hdr, sizeof(hdr), 1, tracefile);

    atexit (trace_cleanup);
    tracefile_enabled = 1;
    return 0;
}

void
trace_push_entry () {
    if (++trace_current == trace_entries + MAX_TRACE_ENTRIES
        || tracefile_nobuf)
        trace_flush();
}
