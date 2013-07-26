
/*--------------------------------------------------------------------*/
/*--- Coverage: Execution traces for GNATcoverage.    cov_traces.c ---*/
/*--------------------------------------------------------------------*/

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

#include <stdlib.h>
#include <string.h>

#include "pub_tool_basics.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_vki.h"        /* vki_stat */
#include "pub_tool_libcfile.h"   /* VG_(write) */
#include "pub_tool_libcbase.h"   /* VG_(memcmp)() */
#include "pub_tool_libcprint.h" /* VG_(messages) */
#include "pub_tool_libcassert.h" /* VG_(exit) */
#include "pub_tool_mallocfree.h" /* plain_free */
#include "cov_traces.h"

/* Legal values for e_machine (architecture).  */

#define EM_NONE        0        /* No machine */
#define EM_M32         1        /* AT&T WE 32100 */
#define EM_SPARC       2        /* SUN SPARC */
#define EM_386         3        /* Intel 80386 */
#define EM_68K         4        /* Motorola m68k family */
#define EM_88K         5        /* Motorola m88k family */
#define EM_486         6        /* Intel 80486 */
#define EM_860         7        /* Intel 80860 */
#define EM_MIPS        8        /* MIPS R3000 big-endian */
#define EM_S370        9        /* Amdahl */
#define EM_MIPS_RS4_BE 10       /* MIPS R4000 big-endian */
#define EM_RS6000      11       /* RS6000 */
#define EM_X86_64      62       /* AMD x86-64 architecture */

#if defined(VGP_x86_linux)
#  define ELF_MACHINE      EM_386
#elif defined(VGP_amd64_linux)
#  define ELF_MACHINE      EM_X86_64
#elif defined(VGP_ppc32_linux)
#  define ELF_MACHINE      EM_PPC
#elif defined(VGP_ppc64_linux)
#  define ELF_MACHINE      EM_PPC64
#elif defined(VGP_arm_linux)
#  define ELF_MACHINE      EM_ARM
#elif defined(VGP_s390x_linux)
#  define ELF_MACHINE      EM_S390
#else
#  error Unknown platform
#endif

#define bswap_16(x)                                             \
({                                                              \
    uint16_t __x = (x);                                         \
    ((uint16_t)(                                                \
        (((uint16_t)(__x) & (uint16_t)0x00ffU) << 8) |          \
        (((uint16_t)(__x) & (uint16_t)0xff00U) >> 8) ));        \
 })

#define bswap_32(x)                                             \
({                                                              \
    uint32_t __x = (x);                                         \
    ((uint32_t)(                                                \
        (((uint32_t)(__x) & (uint32_t)0x000000ffUL) << 24) |    \
        (((uint32_t)(__x) & (uint32_t)0x0000ff00UL) <<  8) |    \
        (((uint32_t)(__x) & (uint32_t)0x00ff0000UL) >>  8) |    \
        (((uint32_t)(__x) & (uint32_t)0xff000000UL) >> 24) ));  \
 })

#define bswap_64(x)                                                     \
({                                                                      \
    uint64_t __x = (x);                                                 \
    ((uint64_t)(                                                        \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x00000000000000ffULL) << 56) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x000000000000ff00ULL) << 40) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x0000000000ff0000ULL) << 24) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x00000000ff000000ULL) <<  8) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x000000ff00000000ULL) >>  8) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x0000ff0000000000ULL) >> 24) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0x00ff000000000000ULL) >> 40) | \
        (uint64_t)(((uint64_t)(__x) & (uint64_t)0xff00000000000000ULL) >> 56) )); \
 })


static Int tracefile;

struct trace_entry *te_list_head = NULL;
struct trace_entry *te_list_tail = NULL;

int                 tracefile_enabled;
static int          tracefile_nobuf;
static int          tracefile_history;

static int    nbr_histmap_entries;
static HWord *histmap_entries;

int tracefile_history_for_te(struct trace_entry *te)
{
    if (!(te->op & TRACE_OP_HIST_CACHE)) {
        tracefile_history_for_te_search(te);
    }

#ifdef DEBUG
    if (te->op & TRACE_OP_HIST_SET) {
        VG_(message)(Vg_FailMsg, "At pc:0x%08x size:0x%08x Hist_Set\n",
                     te->pc, te->size);
    }
#endif

    return te->op & TRACE_OP_HIST_SET;
}

void tracefile_history_for_te_search(struct trace_entry *te)
{
    te->op |= TRACE_OP_HIST_CACHE;

    if (tracefile_history) {
        te->op |= TRACE_OP_HIST_SET;
        return;
    }

    if (nbr_histmap_entries) {
        int low  = 0;
        int high = nbr_histmap_entries - 1;

        while (low <= high) {
            int   mid = low + (high - low) / 2;
            HWord pc  = histmap_entries[mid];

            if (pc >= te->pc && pc < te->pc + te->size) {
                te->op |= TRACE_OP_HIST_SET;
                return;
            }
            if (te->pc < pc) {
                high = mid - 1;
            } else {
                low = mid + 1;
            }
        }
    }
}

static void trace_flush(void)
{
    struct trace_entry32 e32;
    struct trace_entry64 e64;
    struct trace_entry *ret;

    unsigned int ent_sz;
    void *ent;

    if (sizeof(HWord) == 4) {
        ent_sz = sizeof(e32);
        ent = &e32;
    } else {
        ent_sz = sizeof(e64);
        ent = &e64;
    }

    for (ret = te_list_head; ret != NULL; ret = ret->next) {
        /* Write trace entries with non-null op */
        if (ret->op != 0) {
            if (sizeof (HWord) == 4) {
                e32.pc = ret->pc;
                e32.size = ret->size;
                e32.op = ret->op;
            } else {
                e64.pc = ret->pc;
                e64.size = ret->size;
                e64.op = ret->op;
            }
            VG_(write)(tracefile, ent, ent_sz);
        }
    }
}

void trace_cleanup(void)
{
    if (tracefile_enabled) {
        trace_flush();
        VG_(close)(tracefile);
    }
}

static void read_map_file(const char *filename)
{
    Int histfile;
    struct trace_header hdr;
    off_t length;
    int ent_sz;
    int i;
    int my_endian;

    histfile = VG_(fd_open)(filename, VKI_O_RDONLY,
                            VKI_S_IRUSR|VKI_S_IWUSR|VKI_S_IRGRP|VKI_S_IWGRP);
    if (histfile < 0) {
        VG_(message)(Vg_FailMsg, "cannot open histmap file '%s': %m\n",
                     filename);
        VG_(exit)(1);
    }
    if (VG_(read)(histfile, &hdr, sizeof(hdr)) != sizeof(hdr)) {
        VG_(message)(Vg_FailMsg,
                     "cannot read trace header for histmap file '%s'\n",
                     filename);
        VG_(exit)(1);
    }
    if (VG_(memcmp)(hdr.magic, QEMU_TRACE_MAGIC, sizeof(hdr.magic)) != 0
        || hdr.version != QEMU_TRACE_VERSION
        || hdr.kind != QEMU_TRACE_KIND_DECISION_MAP
        || hdr.sizeof_target_pc != sizeof(HWord)
        || (hdr.big_endian != 0 && hdr.big_endian != 1)
        || hdr.machine[0] != (ELF_MACHINE >> 8)
        || hdr.machine[1] != (ELF_MACHINE & 0xff)
        || hdr._pad != 0) {
        VG_(message)(Vg_FailMsg, "bad header for histmap file '%s'\n",
                     filename);
        VG_(exit)(1);
    }

    /* Get number of entries. */
    if ((length = VG_(lseek)(histfile, 0, VKI_SEEK_END)) == -1) {
        VG_(message)(Vg_FailMsg, "cannot get size of histmap file '%s'\n",
                     filename);

        VG_(exit)(1);
    }

    if (VG_(lseek)(histfile, sizeof(hdr), VKI_SEEK_SET) == -1) {
        VG_(message)(Vg_FailMsg, "cannot set seek of histmap file '%s'\n",
                     filename);

        VG_(exit)(1);
    }

    length -= sizeof(hdr);
    if (sizeof(HWord) == 4) {
        ent_sz = sizeof(struct trace_entry32);
    } else {
        ent_sz = sizeof(struct trace_entry64);
    }

    if ((length % ent_sz) != 0) {
        VG_(message)(Vg_FailMsg, "bad length of histmap file '%s'\n", filename);
        VG_(exit)(1);
    }
    nbr_histmap_entries = length / ent_sz;
    if (nbr_histmap_entries) {
        histmap_entries = VG_(malloc)("cov_histmap_entries",
                                      nbr_histmap_entries * sizeof(HWord));
    }

#ifdef WORDS_BIGENDIAN
    my_endian = 1;
#else
    my_endian = 0;
#endif

    if (sizeof(HWord) == 4) {
        for (i = 0; i < nbr_histmap_entries; i++) {
            struct trace_entry32 ent;

            if (VG_(read)(histfile, &ent, sizeof(ent)) != sizeof(ent)) {
                VG_(message)(Vg_FailMsg,
                             "cannot read histmap file entry from '%s'\n",
                             filename);
                VG_(exit)(1);
            }
            if (my_endian != hdr.big_endian) {
                ent.pc = bswap_32(ent.pc);
            }
            if (i > 0 && ent.pc < histmap_entries[i - 1]) {
                VG_(message)(Vg_FailMsg,
                             "unordered entry #%d in histmap file '%s'\n",
                             i, filename);
                VG_(exit)(1);
            }

            histmap_entries[i] = ent.pc;
#ifdef DEBUG
            VG_(message)(Vg_FailMsg, "histmap_entries[%d] = 0x%08x;\n",
                         i, ent.pc);
#endif

        }
    } else {
        for (i = 0; i < nbr_histmap_entries; i++) {
            struct trace_entry64 ent;

            if (VG_(read)(histfile, &ent, sizeof(ent)) != sizeof(ent)) {
                VG_(message)(Vg_FailMsg,
                             "cannot read histmap file entry from '%s'\n",
                             filename);
                VG_(exit)(1);
            }
            if (my_endian != hdr.big_endian) {
                ent.pc = bswap_64(ent.pc);
            }
            if (i > 0 && ent.pc < histmap_entries[i - 1]) {
                VG_(message)(Vg_FailMsg,
                             "unordered entry #%d in histmap file '%s'\n",
                             i, filename);
                VG_(exit)(1);
            }

            histmap_entries[i] = ent.pc;
#ifdef DEBUG
            VG_(message)(Vg_DebugMsg, "histmap_entries[%d] = 0x%016llx;\n",
                         i, (unsigned int)ent.pc);
#endif
        }
    }
    VG_(close)(histfile);
}

void trace_init(const char *cov_exec_arg)
{
    static struct trace_header hdr  = { QEMU_TRACE_MAGIC };
    static int opt_trace_seen;
    int noappend = 0;
    int kind = QEMU_TRACE_KIND_RAW;
    char **s_array = NULL;
    char *cov_exec_file = NULL;
    int s_index = 0;


    if (cov_exec_arg == NULL) {
        /* No arg: trace file disabled */
        return;
    }

    if (opt_trace_seen) {
        VG_(message)(Vg_FailMsg, "option --cov-exec-file already specified\n");
        VG_(exit)(1);
    }
    opt_trace_seen = 1;

    /* Parse coverage argument */
    s_array = split_trace_arg(cov_exec_arg);

    for (s_index = 0; s_array[s_index] != NULL; s_index++) {
        if (VG_(strncmp)(s_array[s_index], "nobuf", 6) == 0) {
            tracefile_nobuf = 1;
        } else if (VG_(strncmp)(s_array[s_index], "history", 8) == 0) {
            tracefile_history = 1;
            kind = QEMU_TRACE_KIND_HISTORY;
        } else if (VG_(strncmp)(s_array[s_index], "noappend", 9) == 0) {
            noappend = 1;
        } else if (VG_(strncmp)(s_array[s_index], "histmap=", 8) == 0) {
            read_map_file(s_array[s_index] + 8);
            kind = QEMU_TRACE_KIND_HISTORY;
        } else {
            cov_exec_file = s_array[s_index];
        }
    }

    tracefile = VG_(fd_open)(cov_exec_file,
                             VKI_O_WRONLY | VKI_O_CREAT | (noappend ? 0 : VKI_O_APPEND),
                             VKI_S_IRUSR|VKI_S_IWUSR|VKI_S_IRGRP|VKI_S_IWGRP);


    if (tracefile < 0) {
        VG_(message)(Vg_FailMsg, "can't open file '%s': %m\n", cov_exec_file);
        VG_(exit)(1);
    }

    hdr.version = QEMU_TRACE_VERSION;
    hdr.sizeof_target_pc = sizeof(HWord);
    hdr.kind = kind;
#ifdef WORDS_BIGENDIAN
    hdr.big_endian = 1;
#else
    hdr.big_endian = 0;
#endif
    hdr.machine[0] = ELF_MACHINE >> 8;
    hdr.machine[1] = ELF_MACHINE & 0xff;
    if (VG_(write)(tracefile, &hdr, sizeof(hdr)) != sizeof(hdr)) {
        VG_(message)(Vg_FailMsg, "can't write trace header on %s\n",
                     cov_exec_file);
        VG_(exit)(1);
    }

    tracefile_enabled = 1;
}

struct trace_entry *new_trace_entry(HWord pc, uint16_t size)
{
    struct trace_entry *ret;

    ret = VG_(malloc)("cov_malloc", sizeof(struct trace_entry));
    ret->pc = pc;
    ret->size = size;
    ret->op = 0;
    ret->next = NULL;

    /* Insert to tail to keep execution order */
    if (te_list_head == NULL) {
        /* First element */
        te_list_head = ret;
        te_list_tail = ret;
    } else {
        te_list_tail->next = ret;
        te_list_tail = ret;
    }
    return ret;
}

struct trace_entry *get_trace_entry(HWord pc, uint16_t size)
{
    struct trace_entry *ret;

    for (ret = te_list_head; ret != NULL; ret = ret->next) {
        if (ret != NULL && ret->pc == pc && ret->size == size) {
            return ret;
        }
    }

    return new_trace_entry(pc, size);
}

char **split_trace_arg(const char *trace)
{
    char **s_array = NULL;
    int s_nbr = 0;
    int start = 0;
    int end = 0;

    for (; 1; end++) {
        if (trace[end] == ',' || trace[end] == '\0') {
            /* split here */

            if (start == end) {
                /* empty string: skip */
                continue;
            }

            /* Make room for a new sub-string */
            s_nbr++;
            s_array = VG_(realloc)("cov_malloc", s_array,
                                   sizeof(char *) * s_nbr);

            /* Allocate and copy the new sub-string */
            s_array[s_nbr - 1] = VG_(malloc)("cov_malloc",
                                             sizeof(char) * (end - start + 1));
            VG_(memmove)(s_array[s_nbr - 1], &trace[start], end - start);
            /* Add the trailing \0 to make valid null-terminated string */
            s_array[s_nbr - 1][end - start] = '\0';

            /* New start point after the end of the current sub-string */
            start = end + 1;
        }
        if (trace[end] == '\0') {
            break;
        }
    }

    /* Null terminated array */
    s_nbr++;
    s_array = VG_(realloc)("cov_malloc", s_array,
                           sizeof(char *) * s_nbr);
    s_array[s_nbr - 1] = NULL;

    return s_array;
}
