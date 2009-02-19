#include <stdio.h>
#include <stdlib.h>
#include "qemu-common.h"
#include "config.h"
#include "qemu-traces.h"
#include "elf.h"

extern struct trace_entry trace_entries[];

void trace_flush(void)
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

void trace_init (void)
{
    static struct trace_header hdr = { QEMU_TRACE_MAGIC };

    //memset(&hdr, 0, sizeof(hdr));
    //memcpy(hdr.magic, QEMU_TRACE_MAGIC, sizeof(hdr.magic));
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
}

