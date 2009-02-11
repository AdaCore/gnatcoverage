/* Qemu trace file format.
   It requires proper definition for uintXX_t where XX is 8, 16, 32 and 64
   and target_ulong (32 or 64 bits).  */

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
#define QEMU_TRACE_KIND_RAW 0
#define QEMU_TRACE_KIND_HISTORY 1

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

/* _BLOCK means pc .. pc+size-1 was executed.  */
#define TRACE_OP_BLOCK 0x10
#define TRACE_OP_FAULT 0x20
#define TRACE_OP_BR0 0x01 /* Branch 0 taken at pc.  */
#define TRACE_OP_BR1 0x02
#define TRACE_OP_BR2 0x04
#define TRACE_OP_BR3 0x08


#endif /* QEMU_TRACE_H */
