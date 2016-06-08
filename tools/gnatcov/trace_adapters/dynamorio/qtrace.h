#ifndef QTRACE_H
#define QTRACE_H

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

typedef unsigned long pctype32;

struct trace_entry32 {
    pctype32 pc;
    unsigned short size;
    unsigned char op;
    unsigned char  _pad[1];
};

typedef unsigned long long pctype64;

struct trace_entry64 {
  pctype64 pc;
  unsigned short size;
  unsigned char  op;
  unsigned char  _pad[5];
};

/* _BLOCK means pc .. pc+size-1 was executed.  */
#define TRACE_OP_BLOCK 0x10     /* Block fully executed.  */
#define TRACE_OP_FAULT 0x20     /* Fault at pc.  */
#define TRACE_OP_BR0   0x01     /* Branch */
#define TRACE_OP_BR1   0x02     /* Fallthrough */

#define TRACE_OP_HIST_SET 0x80

#ifdef X86_32
#  define ELF_MACHINE      EM_386
typedef struct trace_entry32 trace_entry;
typedef pctype32 pctype;
#elif defined(X86_64)
#  define ELF_MACHINE      EM_X86_64
typedef struct trace_entry64 trace_entry;
typedef pctype64 pctype;
#else
#  error "unhandled machine"
#endif

#endif
