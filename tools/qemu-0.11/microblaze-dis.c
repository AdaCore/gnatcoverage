/* Disassemble Xilinx microblaze instructions.
   Copyright (C) 1993, 1999, 2000 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/*
 * Copyright (c) 2001 Xilinx, Inc.  All rights reserved. 
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Xilinx, Inc.  The name of the Company may not be used to endorse 
 * or promote products derived from this software without specific prior 
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	Xilinx, Inc.
 */


#include <stdio.h>
#define STATIC_TABLE
#define DEFINE_TABLE

#ifndef MICROBLAZE_OPC
#define MICROBLAZE_OPC
/* Assembler instructions for Xilinx's microblaze processor
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

   
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/*
 * Copyright (c) 2001 Xilinx, Inc.  All rights reserved. 
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Xilinx, Inc.  The name of the Company may not be used to endorse 
 * or promote products derived from this software without specific prior 
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	Xilinx, Inc.
 */


#ifndef MICROBLAZE_OPCM
#define MICROBLAZE_OPCM

/*
 * Copyright (c) 2001 Xilinx, Inc.  All rights reserved. 
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Xilinx, Inc.  The name of the Company may not be used to endorse 
 * or promote products derived from this software without specific prior 
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	Xilinx, Inc.
 * $Header:
 */

enum microblaze_instr {
   add, rsub, addc, rsubc, addk, rsubk, addkc, rsubkc, cmp, cmpu,
   addi, rsubi, addic, rsubic, addik, rsubik, addikc, rsubikc, mul,
   idiv, idivu, bsll, bsra, bsrl, get, put, nget, nput, cget, cput,
   ncget, ncput, muli, bslli, bsrai, bsrli, mului, or, and, xor,
   andn, pcmpbf, pcmpbc, pcmpeq, pcmpne, sra, src, srl, sext8, sext16, wic, wdc, mts, mfs, br, brd,
   brld, bra, brad, brald, microblaze_brk, beq, beqd, bne, bned, blt,
   bltd, ble, bled, bgt, bgtd, bge, bged, ori, andi, xori, andni,
   imm, rtsd, rtid, rtbd, rted, bri, brid, brlid, brai, braid, bralid,
   brki, beqi, beqid, bnei, bneid, blti, bltid, blei, bleid, bgti,
   bgtid, bgei, bgeid, lbu, lhu, lw, sb, sh, sw, lbui, lhui, lwi,
   sbi, shi, swi, msrset, msrclr, tuqula, fadd, frsub, fmul, fdiv, 
   fcmp_lt, fcmp_eq, fcmp_le, fcmp_gt, fcmp_ne, fcmp_ge, fcmp_un, invalid_inst } ;

enum microblaze_instr_type {
   arithmetic_inst, logical_inst, mult_inst, div_inst, branch_inst,
   return_inst, immediate_inst, special_inst, memory_load_inst,
   memory_store_inst, barrel_shift_inst, anyware_inst };

#define INST_WORD_SIZE 4

/* gen purpose regs go from 0 to 31 */
/* mask is reg num - max_reg_num, ie reg_num - 32 in this case */

#define REG_PC_MASK 0x8000
#define REG_MSR_MASK 0x8001
#define REG_EAR_MASK 0x8003
#define REG_ESR_MASK 0x8005
#define REG_FSR_MASK 0x8007

#define MIN_REGNUM 0
#define MAX_REGNUM 31

#define REG_PC  32 /* PC */
#define REG_MSR 33 /* machine status reg */
#define REG_EAR 35 /* Exception reg */
#define REG_ESR 37 /* Exception reg */
#define REG_FSR 39 /* FPU Status reg */

/* alternate names for gen purpose regs */
#define REG_SP  1 /* stack pointer */
#define REG_ROSDP 2 /* read-only small data pointer */
#define REG_RWSDP 13 /* read-write small data pointer */

/* Assembler Register - Used in Delay Slot Optimization */
#define REG_AS    18
#define REG_ZERO  0
 
#define RD_LOW  21 /* low bit for RD */
#define RA_LOW  16 /* low bit for RA */
#define RB_LOW  11 /* low bit for RB */
#define IMM_LOW  0 /* low bit for immediate */

#define RD_MASK 0x03E00000
#define RA_MASK 0x001F0000
#define RB_MASK 0x0000F800
#define IMM_MASK 0x0000FFFF

// imm mask for barrel shifts
#define IMM5_MASK 0x0000001F


// imm mask for get, put instructions
#define  IMM12_MASK 0x00000FFF

// imm mask for msrset, msrclr instructions
#define  IMM14_MASK 0x00003FFF

#endif /* MICROBLAZE-OPCM */

#define INST_TYPE_RD_R1_R2 0
#define INST_TYPE_RD_R1_IMM 1
#define INST_TYPE_RD_R1_UNSIGNED_IMM 2
#define INST_TYPE_RD_R1 3
#define INST_TYPE_RD_R2 4
#define INST_TYPE_RD_IMM 5
#define INST_TYPE_R2 6
#define INST_TYPE_R1_R2 7
#define INST_TYPE_R1_IMM 8
#define INST_TYPE_IMM 9
#define INST_TYPE_SPECIAL_R1 10
#define INST_TYPE_RD_SPECIAL 11
#define INST_TYPE_R1 12
  // new instn type for barrel shift imms
#define INST_TYPE_RD_R1_IMM5  13
#define INST_TYPE_RD_IMM12    14
#define INST_TYPE_R1_IMM12    15

  // new insn type for insn cache
#define INST_TYPE_RD_R1_SPECIAL 16

// new insn type for msrclr, msrset insns.
#define INST_TYPE_RD_IMM14    17

// new insn type for tuqula rd - addik rd, r0, 42
#define INST_TYPE_RD    18

#define INST_TYPE_NONE 25



#define INST_PC_OFFSET 1 /* instructions where the label address is resolved as a PC offset (for branch label)*/
#define INST_NO_OFFSET 0 /* instructions where the label address is resolved as an absolute value (for data mem or abs address)*/

#define IMMVAL_MASK_NON_SPECIAL 0x0000
#define IMMVAL_MASK_MTS 0x4000
#define IMMVAL_MASK_MFS 0x0000

#define OPCODE_MASK_H   0xFC000000 /* High 6 bits only */
#define OPCODE_MASK_H1  0xFFE00000 /* High 11 bits */
#define OPCODE_MASK_H2  0xFC1F0000 /* High 6 and bits 20-16 */
#define OPCODE_MASK_H12 0xFFFF0000 /* High 16 */
#define OPCODE_MASK_H4  0xFC0007FF /* High 6 and low 11 bits */
#define OPCODE_MASK_H13S 0xFFE0FFF0 /* High 11 and 15:1 bits and last nibble of last byte for spr */
#define OPCODE_MASK_H23S 0xFC1FFFF0 /* High 6, 20-16 and 15:1 bits and last nibble of last byte for spr */
#define OPCODE_MASK_H34 0xFC00FFFF /* High 6 and low 16 bits */
#define OPCODE_MASK_H14 0xFFE007FF /* High 11 and low 11 bits */
#define OPCODE_MASK_H24 0xFC1F07FF /* High 6, bits 20-16 and low 11 bits */
#define OPCODE_MASK_H124  0xFFFF07FF /* High 16, and low 11 bits */
#define OPCODE_MASK_H1234 0xFFFFFFFF /* All 32 bits */
#define OPCODE_MASK_H3  0xFC000600 /* High 6 bits and bits 21, 22 */  
#define OPCODE_MASK_H32 0xFC00F000 /* High 6 bits and bit 16, 17, 18 and 19*/
#define OPCODE_MASK_H34B   0xFC0000FF /* High 6 bits and low 8 bits */

// New Mask for msrset, msrclr insns.
#define OPCODE_MASK_H23N  0xFC1FC000 /* High 6 and bits 12 - 18 */

#define DELAY_SLOT 1
#define NO_DELAY_SLOT 0

#define MAX_OPCODES 149

struct op_code_struct {
  const char *name;
  short inst_type; /* registers and immediate values involved */
  short inst_offset_type; /* immediate vals offset from PC? (= 1 for branches) */
  short delay_slots; /* info about delay slots needed after this instr. */
  short immval_mask;
  unsigned long bit_sequence; /* all the fixed bits for the op are set and all the variable bits (reg names, imm vals) are set to 0 */ 
  unsigned long opcode_mask; /* which bits define the opcode */
  enum microblaze_instr instr;
  enum microblaze_instr_type instr_type;
  /* more info about output format here */
} opcodes[MAX_OPCODES] = 

{ 
  {"add",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x00000000, OPCODE_MASK_H4, add, arithmetic_inst },
  {"rsub",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x04000000, OPCODE_MASK_H4, rsub, arithmetic_inst },
  {"addc",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x08000000, OPCODE_MASK_H4, addc, arithmetic_inst },
  {"rsubc", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x0C000000, OPCODE_MASK_H4, rsubc, arithmetic_inst },
  {"addk",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x10000000, OPCODE_MASK_H4, addk, arithmetic_inst },
  {"rsubk", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x14000000, OPCODE_MASK_H4, rsubk, arithmetic_inst },
  {"cmp",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x14000001, OPCODE_MASK_H4, cmp, arithmetic_inst },
  {"cmpu",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x14000003, OPCODE_MASK_H4, cmpu, arithmetic_inst },
  {"addkc", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x18000000, OPCODE_MASK_H4, addkc, arithmetic_inst },
  {"rsubkc",INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x1C000000, OPCODE_MASK_H4, rsubkc, arithmetic_inst },
  {"addi",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x20000000, OPCODE_MASK_H, addi, arithmetic_inst },
  {"rsubi", INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x24000000, OPCODE_MASK_H, rsubi, arithmetic_inst },
  {"addic", INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x28000000, OPCODE_MASK_H, addic, arithmetic_inst },
  {"rsubic",INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x2C000000, OPCODE_MASK_H, rsubic, arithmetic_inst },
  {"addik", INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x30000000, OPCODE_MASK_H, addik, arithmetic_inst },
  {"rsubik",INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x34000000, OPCODE_MASK_H, rsubik, arithmetic_inst },
  {"addikc",INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x38000000, OPCODE_MASK_H, addikc, arithmetic_inst },
  {"rsubikc",INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x3C000000, OPCODE_MASK_H, rsubikc, arithmetic_inst },
  {"mul",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x40000000, OPCODE_MASK_H4, mul, mult_inst },
  {"idiv",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x48000000, OPCODE_MASK_H4, idiv, div_inst },
  {"idivu", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x48000002, OPCODE_MASK_H4, idivu, div_inst },
  {"bsll",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x44000400, OPCODE_MASK_H3, bsll, barrel_shift_inst },
  {"bsra",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x44000200, OPCODE_MASK_H3, bsra, barrel_shift_inst },
  {"bsrl",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x44000000, OPCODE_MASK_H3, bsrl, barrel_shift_inst },
  {"get",   INST_TYPE_RD_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C000000, OPCODE_MASK_H32, get, anyware_inst },
  {"put",   INST_TYPE_R1_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C008000, OPCODE_MASK_H32, put, anyware_inst },
  {"nget",  INST_TYPE_RD_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C004000, OPCODE_MASK_H32, nget, anyware_inst },
  {"nput",  INST_TYPE_R1_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C00C000, OPCODE_MASK_H32, nput, anyware_inst },
  {"cget",  INST_TYPE_RD_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C002000, OPCODE_MASK_H32, cget, anyware_inst },
  {"cput",  INST_TYPE_R1_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C00A000, OPCODE_MASK_H32, cput, anyware_inst },
  {"ncget", INST_TYPE_RD_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C006000, OPCODE_MASK_H32, ncget, anyware_inst },
  {"ncput", INST_TYPE_R1_IMM12, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x6C00E000, OPCODE_MASK_H32, ncput, anyware_inst },
  {"muli",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x60000000, OPCODE_MASK_H, muli, mult_inst },
  {"bslli", INST_TYPE_RD_R1_IMM5, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x64000400, OPCODE_MASK_H3, bslli, barrel_shift_inst },
  {"bsrai", INST_TYPE_RD_R1_IMM5, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x64000200, OPCODE_MASK_H3, bsrai, barrel_shift_inst },
  {"bsrli", INST_TYPE_RD_R1_IMM5, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x64000000, OPCODE_MASK_H3, bsrli, barrel_shift_inst },
  {"or",    INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x80000000, OPCODE_MASK_H4, or, logical_inst },
  {"and",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x84000000, OPCODE_MASK_H4, and, logical_inst },
  {"xor",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x88000000, OPCODE_MASK_H4, xor, logical_inst },
  {"andn",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x8C000000, OPCODE_MASK_H4, andn, logical_inst },
  {"pcmpbf",INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x80000400, OPCODE_MASK_H4, pcmpbf, logical_inst },
  {"pcmpbc",INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x84000400, OPCODE_MASK_H4, pcmpbc, logical_inst },
  {"pcmpeq",INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x88000400, OPCODE_MASK_H4, pcmpeq, logical_inst },
  {"pcmpne",INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x8C000400, OPCODE_MASK_H4, pcmpne, logical_inst },
  {"sra",   INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000001, OPCODE_MASK_H34, sra, logical_inst },
  {"src",   INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000021, OPCODE_MASK_H34, src, logical_inst },
  {"srl",   INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000041, OPCODE_MASK_H34, srl, logical_inst },
  {"sext8", INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000060, OPCODE_MASK_H34, sext8, logical_inst },
  {"sext16",INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000061, OPCODE_MASK_H34, sext16, logical_inst },
  {"wic",   INST_TYPE_RD_R1_SPECIAL, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000068, OPCODE_MASK_H34B, wic, special_inst },
  {"wdc",   INST_TYPE_RD_R1_SPECIAL, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x90000064, OPCODE_MASK_H34B, wdc, special_inst },
  {"mts",   INST_TYPE_SPECIAL_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_MTS, 0x9400C000, OPCODE_MASK_H13S, mts, special_inst },
  {"mfs",   INST_TYPE_RD_SPECIAL, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_MFS, 0x94008000, OPCODE_MASK_H23S, mfs, special_inst },
  {"br",    INST_TYPE_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x98000000, OPCODE_MASK_H124, br, branch_inst },
  {"brd",   INST_TYPE_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x98100000, OPCODE_MASK_H124, brd, branch_inst },
  {"brld",  INST_TYPE_RD_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x98140000, OPCODE_MASK_H24, brld, branch_inst },
  {"bra",   INST_TYPE_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x98080000, OPCODE_MASK_H124, bra, branch_inst },
  {"brad",  INST_TYPE_R2, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x98180000, OPCODE_MASK_H124, brad, branch_inst },
  {"brald", INST_TYPE_RD_R2, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x981C0000, OPCODE_MASK_H24, brald, branch_inst },
  {"brk",   INST_TYPE_RD_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x980C0000, OPCODE_MASK_H24, microblaze_brk, branch_inst },
  {"beq",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9C000000, OPCODE_MASK_H14, beq, branch_inst },
  {"beqd",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9E000000, OPCODE_MASK_H14, beqd, branch_inst },
  {"bne",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9C200000, OPCODE_MASK_H14, bne, branch_inst },
  {"bned",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9E200000, OPCODE_MASK_H14, bned, branch_inst },
  {"blt",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9C400000, OPCODE_MASK_H14, blt, branch_inst },
  {"bltd",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9E400000, OPCODE_MASK_H14, bltd, branch_inst },
  {"ble",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9C600000, OPCODE_MASK_H14, ble, branch_inst },
  {"bled",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9E600000, OPCODE_MASK_H14, bled, branch_inst },
  {"bgt",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9C800000, OPCODE_MASK_H14, bgt, branch_inst },
  {"bgtd",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9E800000, OPCODE_MASK_H14, bgtd, branch_inst },
  {"bge",   INST_TYPE_R1_R2, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9CA00000, OPCODE_MASK_H14, bge, branch_inst },
  {"bged",  INST_TYPE_R1_R2, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x9EA00000, OPCODE_MASK_H14, bged, branch_inst },
  {"ori",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xA0000000, OPCODE_MASK_H, ori, logical_inst },
  {"andi",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xA4000000, OPCODE_MASK_H, andi, logical_inst },
  {"xori",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xA8000000, OPCODE_MASK_H, xori, logical_inst },
  {"andni", INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xAC000000, OPCODE_MASK_H, andni, logical_inst },
  {"imm",   INST_TYPE_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB0000000, OPCODE_MASK_H12, imm, immediate_inst },
  {"rtsd",  INST_TYPE_R1_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB6000000, OPCODE_MASK_H1, rtsd, return_inst },
  {"rtid",  INST_TYPE_R1_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB6200000, OPCODE_MASK_H1, rtid, return_inst },
  {"rtbd",  INST_TYPE_R1_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB6400000, OPCODE_MASK_H1, rtbd, return_inst },
  {"rted",  INST_TYPE_R1_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB6800000, OPCODE_MASK_H1, rted, return_inst },
  {"bri",   INST_TYPE_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB8000000, OPCODE_MASK_H12, bri, branch_inst },
  {"brid",  INST_TYPE_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB8100000, OPCODE_MASK_H12, brid, branch_inst },
  {"brlid", INST_TYPE_RD_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB8140000, OPCODE_MASK_H2, brlid, branch_inst },
  {"brai",  INST_TYPE_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB8080000, OPCODE_MASK_H12, brai, branch_inst },
  {"braid", INST_TYPE_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB8180000, OPCODE_MASK_H12, braid, branch_inst },
  {"bralid",INST_TYPE_RD_IMM, INST_NO_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB81C0000, OPCODE_MASK_H2, bralid, branch_inst },
  {"brki",  INST_TYPE_RD_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB80C0000, OPCODE_MASK_H2, brki, branch_inst },
  {"beqi",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBC000000, OPCODE_MASK_H1, beqi, branch_inst },
  {"beqid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBE000000, OPCODE_MASK_H1, beqid, branch_inst },
  {"bnei",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBC200000, OPCODE_MASK_H1, bnei, branch_inst },
  {"bneid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBE200000, OPCODE_MASK_H1, bneid, branch_inst },
  {"blti",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBC400000, OPCODE_MASK_H1, blti, branch_inst },
  {"bltid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBE400000, OPCODE_MASK_H1, bltid, branch_inst },
  {"blei",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBC600000, OPCODE_MASK_H1, blei, branch_inst },
  {"bleid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBE600000, OPCODE_MASK_H1, bleid, branch_inst },
  {"bgti",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBC800000, OPCODE_MASK_H1, bgti, branch_inst },
  {"bgtid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBE800000, OPCODE_MASK_H1, bgtid, branch_inst },
  {"bgei",  INST_TYPE_R1_IMM, INST_PC_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBCA00000, OPCODE_MASK_H1, bgei, branch_inst },
  {"bgeid", INST_TYPE_R1_IMM, INST_PC_OFFSET, DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xBEA00000, OPCODE_MASK_H1, bgeid, branch_inst },
  {"lbu",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xC0000000, OPCODE_MASK_H4, lbu, memory_load_inst },
  {"lhu",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xC4000000, OPCODE_MASK_H4, lhu, memory_load_inst },
  {"lw",    INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xC8000000, OPCODE_MASK_H4, lw, memory_load_inst },
  {"sb",    INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xD0000000, OPCODE_MASK_H4, sb, memory_store_inst },
  {"sh",    INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xD4000000, OPCODE_MASK_H4, sh, memory_store_inst },
  {"sw",    INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xD8000000, OPCODE_MASK_H4, sw, memory_store_inst },
  {"lbui",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xE0000000, OPCODE_MASK_H, lbui, memory_load_inst },
  {"lhui",  INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xE4000000, OPCODE_MASK_H, lhui, memory_load_inst },
  {"lwi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xE8000000, OPCODE_MASK_H, lwi, memory_load_inst },
  {"sbi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xF0000000, OPCODE_MASK_H, sbi, memory_store_inst },
  {"shi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xF4000000, OPCODE_MASK_H, shi, memory_store_inst },
  {"swi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xF8000000, OPCODE_MASK_H, swi, memory_store_inst },
  {"nop",   INST_TYPE_NONE, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x80000000, OPCODE_MASK_H1234, invalid_inst, logical_inst }, /* translates to or r0, r0, r0 */
  {"la",    INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x30000000, OPCODE_MASK_H, invalid_inst, arithmetic_inst }, /* la translates to addik */
  {"tuqula",INST_TYPE_RD, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x3000002A, OPCODE_MASK_H, invalid_inst, arithmetic_inst }, /* tuqula rd translates to addik rd, r0, 42 */
  {"not",   INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xA800FFFF, OPCODE_MASK_H34, invalid_inst, logical_inst }, /* not translates to xori rd,ra,-1 */
  {"neg",   INST_TYPE_RD_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x04000000, OPCODE_MASK_H, invalid_inst, arithmetic_inst }, /* neg translates to rsub rd, ra, r0 */
  {"rtb",   INST_TYPE_R1, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xB6000004, OPCODE_MASK_H1, invalid_inst, return_inst }, /* rtb translates to rts rd, 4 */
  {"sub",   INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x04000000, OPCODE_MASK_H, invalid_inst, arithmetic_inst }, /* sub translates to rsub rd, rb, ra */
  {"lmi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xE8000000, OPCODE_MASK_H, invalid_inst, memory_load_inst },
  {"smi",   INST_TYPE_RD_R1_IMM, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0xF8000000, OPCODE_MASK_H, invalid_inst, memory_store_inst },
  {"msrset",INST_TYPE_RD_IMM14, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x94100000, OPCODE_MASK_H23N, msrset, special_inst },
  {"msrclr",INST_TYPE_RD_IMM14, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x94110000, OPCODE_MASK_H23N, msrclr, special_inst },
  {"fadd",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000000, OPCODE_MASK_H4, fadd, arithmetic_inst },
  {"frsub",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000080, OPCODE_MASK_H4, frsub, arithmetic_inst },
  {"fmul",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000100, OPCODE_MASK_H4, fmul, arithmetic_inst },
  {"fdiv",  INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000180, OPCODE_MASK_H4, fdiv, arithmetic_inst },
  {"fcmp.lt", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000210, OPCODE_MASK_H4, fcmp_lt, arithmetic_inst },
  {"fcmp.eq", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000220, OPCODE_MASK_H4, fcmp_eq, arithmetic_inst },
  {"fcmp.le", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000230, OPCODE_MASK_H4, fcmp_le, arithmetic_inst },
  {"fcmp.gt", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000240, OPCODE_MASK_H4, fcmp_gt, arithmetic_inst },
  {"fcmp.ne", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000250, OPCODE_MASK_H4, fcmp_ne, arithmetic_inst },
  {"fcmp.ge", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000260, OPCODE_MASK_H4, fcmp_ge, arithmetic_inst },
  {"fcmp.un", INST_TYPE_RD_R1_R2, INST_NO_OFFSET, NO_DELAY_SLOT, IMMVAL_MASK_NON_SPECIAL, 0x58000200, OPCODE_MASK_H4, fcmp_un, arithmetic_inst },
  {""}
};

/* prefix for register names */
char register_prefix[] = "r";
char special_register_prefix[] = "spr";
char fsl_register_prefix[] = "rfsl";


/* #defines for valid immediate range */
#define MIN_IMM  0x80000000
#define MAX_IMM  0x7fffffff 

#define MIN_IMM12  0x000
#define MAX_IMM12  0x7ff

#define MIN_IMM14  0x0000
#define MAX_IMM14  0x1fff

#endif /* MICROBLAZE_OPC */

#include "dis-asm.h"
#include <strings.h>

#define get_field_rd(instr) get_field(instr, RD_MASK, RD_LOW)
#define get_field_r1(instr) get_field(instr, RA_MASK, RA_LOW)
#define get_field_r2(instr) get_field(instr, RB_MASK, RB_LOW)
#define get_int_field_imm(instr) ((instr & IMM_MASK) >> IMM_LOW)
#define get_int_field_r1(instr) ((instr & RA_MASK) >> RA_LOW)

static char *
get_field (long instr, long mask, unsigned short low) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%s%d", register_prefix, (int)((instr & mask) >> low));
  return(strdup(tmpstr));
}

static char *
get_field_imm (long instr) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%d", (short)((instr & IMM_MASK) >> IMM_LOW));
  return(strdup(tmpstr));
}

static char *
get_field_imm5 (long instr) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%d", (short)((instr & IMM5_MASK) >> IMM_LOW));
  return(strdup(tmpstr));
}

static char *
get_field_imm12 (long instr) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%s%d", fsl_register_prefix, (short)((instr & IMM12_MASK) >> IMM_LOW));
  return(strdup(tmpstr));
}

static char *
get_field_imm14 (long instr) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%d", (short)((instr & IMM14_MASK) >> IMM_LOW));
  return(strdup(tmpstr));
}

#if 0
static char *
get_field_unsigned_imm (long instr) 
{
  char tmpstr[25];
  sprintf(tmpstr, "%d", (int)((instr & IMM_MASK) >> IMM_LOW));
  return(strdup(tmpstr));
}
#endif

/*
  char *
  get_field_special (instr) 
  long instr;
  {
  char tmpstr[25];
  
  sprintf(tmpstr, "%s%s", register_prefix, (((instr & IMM_MASK) >> IMM_LOW) & REG_MSR_MASK) == 0 ? "pc" : "msr");
  
  return(strdup(tmpstr));
  }
*/

static char *
get_field_special (long instr, struct op_code_struct * op) 
{
   char tmpstr[25];
   char spr[5];

   switch ( (((instr & IMM_MASK) >> IMM_LOW) ^ op->immval_mask) ) {
   case REG_MSR_MASK :
      strcpy(spr, "msr");
      break;
   case REG_PC_MASK :
      strcpy(spr, "pc");
      break;
   case REG_EAR_MASK :
      strcpy(spr, "ear");
      break;
   case REG_ESR_MASK :
      strcpy(spr, "esr");
      break;
   case REG_FSR_MASK :
      strcpy(spr, "fsr");
      break;      
   default :
      strcpy(spr, "pc");
      break;
   }
   
   sprintf(tmpstr, "%s%s", register_prefix, spr);
   return(strdup(tmpstr));
}

static unsigned long
read_insn_microblaze(bfd_vma memaddr, struct disassemble_info *info,
                     struct op_code_struct ** opr)
{
  unsigned char       ibytes[4];
  int                 status;
  struct op_code_struct * op;
  unsigned long inst;

  status = info->read_memory_func (memaddr, ibytes, 4, info);

  if (status != 0) 
    {
      info->memory_error_func (status, memaddr, info);
      return 0;
    }

  if (info->endian == BFD_ENDIAN_BIG)
    inst = (ibytes[0] << 24) | (ibytes[1] << 16) | (ibytes[2] << 8) | ibytes[3];
  else if (info->endian == BFD_ENDIAN_LITTLE)
    inst = (ibytes[3] << 24) | (ibytes[2] << 16) | (ibytes[1] << 8) | ibytes[0];
  else
    abort ();

  /* Just a linear search of the table.  */
  for (op = opcodes; op->name != 0; op ++)
    if (op->bit_sequence == (inst & op->opcode_mask))
      break;

  *opr = op;
  return inst;
}


int 
print_insn_microblaze (bfd_vma memaddr, struct disassemble_info * info)
{
  fprintf_ftype       fprintf = info->fprintf_func;
  void *              stream = info->stream;
  unsigned long       inst, prev_inst;
  struct op_code_struct * op, *pop;
  int                 immval = 0;
  boolean             immfound = false;
  static bfd_vma prev_insn_addr = -1; /*init the prev insn addr */
  static int     prev_insn_vma = -1;  /*init the prev insn vma */
  int            curr_insn_vma = info->buffer_vma;

  info->bytes_per_chunk = 4;

  inst = read_insn_microblaze (memaddr, info, &op);
  if (inst == 0)
    return -1;
  
  if (prev_insn_vma == curr_insn_vma) {
  if (memaddr-(info->bytes_per_chunk) == prev_insn_addr) {
    prev_inst = read_insn_microblaze (prev_insn_addr, info, &pop);
    if (prev_inst == 0)
      return -1;
    if (pop->instr == imm) {
      immval = (get_int_field_imm(prev_inst) << 16) & 0xffff0000;
      immfound = true;
    }
    else {
      immval = 0;
      immfound = false;
    }
  }
  }
  /* make curr insn as prev insn */
  prev_insn_addr = memaddr;
  prev_insn_vma = curr_insn_vma;

  if (op->name == 0)
    fprintf (stream, ".short 0x%04x", inst);
  else
    {
      fprintf (stream, "%s", op->name);
      
      switch (op->inst_type)
	{
  case INST_TYPE_RD_R1_R2:
     fprintf(stream, "\t%s, %s, %s", get_field_rd(inst), get_field_r1(inst), get_field_r2(inst));
     break;
        case INST_TYPE_RD_R1_IMM:
	  fprintf(stream, "\t%s, %s, %s", get_field_rd(inst), get_field_r1(inst), get_field_imm(inst));
	  if (info->print_address_func && get_int_field_r1(inst) == 0 && info->symbol_at_address_func) {
	    if (immfound)
	      immval |= (get_int_field_imm(inst) & 0x0000ffff);
	    else {
	      immval = get_int_field_imm(inst);
	      if (immval & 0x8000)
		immval |= 0xFFFF0000;
	    }
	    if (immval > 0 && info->symbol_at_address_func(immval, info)) {
	      fprintf (stream, "\t// ");
	      info->print_address_func (immval, info);
	    }
	  }
	  break;
	case INST_TYPE_RD_R1_IMM5:
	  fprintf(stream, "\t%s, %s, %s", get_field_rd(inst), get_field_r1(inst), get_field_imm5(inst));
	  break;
	case INST_TYPE_RD_IMM12:
	  fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_imm12(inst));
	  break;
	case INST_TYPE_R1_IMM12:
	  fprintf(stream, "\t%s, %s", get_field_r1(inst), get_field_imm12(inst));
	  break;
	case INST_TYPE_RD_SPECIAL:
	  fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_special(inst, op));
	  break;
	case INST_TYPE_SPECIAL_R1:
	  fprintf(stream, "\t%s, %s", get_field_special(inst, op), get_field_r1(inst));
	  break;
	case INST_TYPE_RD_R1:
	  fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_r1(inst));
	  break;
	case INST_TYPE_R1_R2:
	  fprintf(stream, "\t%s, %s", get_field_r1(inst), get_field_r2(inst));
	  break;
	case INST_TYPE_R1_IMM:
	  fprintf(stream, "\t%s, %s", get_field_r1(inst), get_field_imm(inst));
	  /* The non-pc relative instructions are returns, which shouldn't 
	     have a label printed */
	  if (info->print_address_func && op->inst_offset_type == INST_PC_OFFSET && info->symbol_at_address_func) {
	    if (immfound)
	      immval |= (get_int_field_imm(inst) & 0x0000ffff);
	    else {
	      immval = get_int_field_imm(inst);
	      if (immval & 0x8000)
		immval |= 0xFFFF0000;
	    }
	    immval += memaddr;
	    if (immval > 0 && info->symbol_at_address_func(immval, info)) {
	      fprintf (stream, "\t// ");
	      info->print_address_func (immval, info);
	    } else {
	      fprintf (stream, "\t\t// ");
	      fprintf (stream, "%x", immval);
	    }
	  }
	  break;
        case INST_TYPE_RD_IMM:
	  fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_imm(inst));
	  if (info->print_address_func && info->symbol_at_address_func) {
	    if (immfound)
	      immval |= (get_int_field_imm(inst) & 0x0000ffff);
	    else {
	      immval = get_int_field_imm(inst);
	      if (immval & 0x8000)
		immval |= 0xFFFF0000;
	    }
	    if (op->inst_offset_type == INST_PC_OFFSET)
	      immval += (int) memaddr;
	    if (info->symbol_at_address_func(immval, info)) {
	      fprintf (stream, "\t// ");
	      info->print_address_func (immval, info);
	    } 
	  }
	  break;
        case INST_TYPE_IMM:
	  fprintf(stream, "\t%s", get_field_imm(inst));
	  if (info->print_address_func && info->symbol_at_address_func && op->instr != imm) {
	    if (immfound)
	      immval |= (get_int_field_imm(inst) & 0x0000ffff);
	    else {
	      immval = get_int_field_imm(inst);
	      if (immval & 0x8000)
		immval |= 0xFFFF0000;
	    }
	    if (op->inst_offset_type == INST_PC_OFFSET)
	      immval += (int) memaddr;
	    if (immval > 0 && info->symbol_at_address_func(immval, info)) {
	      fprintf (stream, "\t// ");
	      info->print_address_func (immval, info);
	    } else if (op->inst_offset_type == INST_PC_OFFSET) {
	      fprintf (stream, "\t\t// ");
	      fprintf (stream, "%x", immval);
	    }
	  }
	  break;
        case INST_TYPE_RD_R2:
	  fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_r2(inst));
	  break;
  case INST_TYPE_R2:
     fprintf(stream, "\t%s", get_field_r2(inst));
     break;
  case INST_TYPE_R1:
     fprintf(stream, "\t%s", get_field_r1(inst));
     break;
  case INST_TYPE_RD_R1_SPECIAL:
     fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_r2(inst));
     break;
  case INST_TYPE_RD_IMM14:
     fprintf(stream, "\t%s, %s", get_field_rd(inst), get_field_imm14(inst));
     break;
     /* For tuqula instruction */
  case INST_TYPE_RD:
     fprintf(stream, "\t%s", get_field_rd(inst));
     break;
     
  default:
	  /* if the disassembler lags the instruction set */
	  fprintf (stream, "\tundecoded operands, inst is 0x%04x", inst);
	  break;
	}
    }
  
  /* Say how many bytes we consumed? */
  return 4;
}

#if 0
static enum microblaze_instr
get_insn_microblaze (long inst, boolean *isunsignedimm,
                     enum microblaze_instr_type *insn_type,
                     short *delay_slots ) 
{
  struct op_code_struct * op;
  *isunsignedimm = false;

  /* Just a linear search of the table.  */
  for (op = opcodes; op->name != 0; op ++)
    if (op->bit_sequence == (inst & op->opcode_mask))
      break;

  if (op->name == 0)
    return invalid_inst;
  else {
    *isunsignedimm = (op->inst_type == INST_TYPE_RD_R1_UNSIGNED_IMM);
    *insn_type = op->instr_type;
    *delay_slots = op->delay_slots;
    return op->instr;
  }
}
#endif

#if 0
static short
get_delay_slots_microblaze ( long inst )
{
  boolean isunsignedimm;
  enum microblaze_instr_type insn_type;
  enum microblaze_instr op;
  short delay_slots;

  op = get_insn_microblaze( inst, &isunsignedimm, &insn_type, &delay_slots);
  if (op == invalid_inst)
    return 0;
  else 
    return delay_slots;
}
#endif

#if 0
static enum microblaze_instr
microblaze_decode_insn (long insn, int *rd, int *ra, int *rb, int *imm)
{
  enum microblaze_instr op;
  boolean t1;
  enum microblaze_instr_type t2;
  short t3;

  op = get_insn_microblaze(insn, &t1, &t2, &t3);
  *rd = (insn & RD_MASK) >> RD_LOW;
  *ra = (insn & RA_MASK) >> RA_LOW;
  *rb = (insn & RB_MASK) >> RB_LOW;
  t3 = (insn & IMM_MASK) >> IMM_LOW;
  *imm = (int) t3;
  return (op);
}
#endif

#if 0
static unsigned long
microblaze_get_target_address (long inst, boolean immfound, int immval,
                               long pcval, long r1val, long r2val,
                               boolean *targetvalid,
                               boolean *unconditionalbranch)
{
  struct op_code_struct * op;
  long targetaddr = 0;

  *unconditionalbranch = false;
  /* Just a linear search of the table.  */
  for (op = opcodes; op->name != 0; op ++)
    if (op->bit_sequence == (inst & op->opcode_mask))
      break;

  if (op->name == 0) {
    *targetvalid = false;
  } else if (op->instr_type == branch_inst) {
    switch (op->inst_type) {
    case INST_TYPE_R2:
      *unconditionalbranch = true;
      /* fallthru */
    case INST_TYPE_RD_R2:
    case INST_TYPE_R1_R2:
      targetaddr = r2val;
      *targetvalid = true;
      if (op->inst_offset_type == INST_PC_OFFSET)
	targetaddr += pcval;
      break;
    case INST_TYPE_IMM:
      *unconditionalbranch = true;
      /* fallthru */
    case INST_TYPE_RD_IMM:
    case INST_TYPE_R1_IMM:
      if (immfound) {
	targetaddr = (immval << 16) & 0xffff0000;
	targetaddr |= (get_int_field_imm(inst) & 0x0000ffff);
      } else {
	targetaddr = get_int_field_imm(inst);
	if (targetaddr & 0x8000)
	  targetaddr |= 0xFFFF0000;
      }
      if (op->inst_offset_type == INST_PC_OFFSET)
	targetaddr += pcval;
      *targetvalid = true;
      break;
    default:
      *targetvalid = false;
      break;
    }
  } else if (op->instr_type == return_inst) {
      if (immfound) {
	targetaddr = (immval << 16) & 0xffff0000;
	targetaddr |= (get_int_field_imm(inst) & 0x0000ffff);
      } else {
	targetaddr = get_int_field_imm(inst);
	if (targetaddr & 0x8000)
	  targetaddr |= 0xFFFF0000;
      }
      targetaddr += r1val;
      *targetvalid = true;
  } else {
    *targetvalid = false;
  }
  return targetaddr;
}
#endif
