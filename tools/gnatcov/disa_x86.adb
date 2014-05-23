------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2006-2013, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  This package uses the same naming convention as the Annex A ("Opcode Map")
--  in Intel's software developper's manual volume 2B, as well as the section
--  A.1 and chapter 2 ("Instruction format") in volume 2A.
--  These manuals can be found at http://www.intel.com/product/manuals/

with Elf_Common;
with Interfaces;   use Interfaces;
with Outputs;      use Outputs;
with Hex_Images;   use Hex_Images;
with Highlighting; use Highlighting;

package body Disa_X86 is

   Debug : constant Boolean := False;

   subtype Byte is Interfaces.Unsigned_8;
   type Bit is mod 2;
   type Bit_Field_2 is mod 2 ** 2;
   type Bit_Field_3 is mod 2 ** 3;
   type Bit_Field_4 is mod 2 ** 4;

   type Width_Type is (W_None, W_8, W_16, W_32, W_64, W_128);
   --  Width for operands, addresses and registers

   type Reg_Class_Type is
     (R_None,
      R_8, R_16, R_32, R_64,
      R_Control, R_Debug,
      R_MM, R_XMM);
   --  Set of registers that can be simultaneously addressed:
   --  - R_8:        AL,   CL,   DL,   BL,   AH,   CH,   DH,   BH
   --  - R_16:       AX,   CX,   DX,   BX,   SP,   BP,   SI,   DI
   --  - R_32:       EAX,  ECX,  EDX,  EBX,  ESP,  EBP,  ESI,  EDI
   --  - R_Control:  CR0,  CR1,  CR2,  CR3,  CR4,  CR5,  CR6,  CR7
   --  - R_Debug:    DR0,  DR1,  DR2,  DR3,  DR4,  DR5,  DR6,  DR7
   --  - R_MM:       MM0,  MM1,  MM2,  MM3,  MM4,  MM5,  MM6,  MM7
   --  - R_XMM:      XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7

   --  The following codes describe the operand kinds used by instruction
   --  descriptors. They indicate whether the ModR/M byte is required, the size
   --  of operands, the involved register class, etc.

   type Code_Type is
     (
      --  This special value indicates that there is no operand.
      C_None,

      --  These are instruction prefixes, not really operands.
      C_Prefix_Cs,
      C_Prefix_Ss,
      C_Prefix_Ds,
      C_Prefix_Es,
      C_Prefix_Fs,
      C_Prefix_Gs,
      C_Prefix_Lock,
      C_Prefix_Rep,
      C_Prefix_Repne,
      C_Prefix_Oper,
      C_Prefix_Addr,
      C_0F,

      --------------------
      -- Start of Modrm --
      --------------------

      --  The following operand types imply the presence of a
      --  ModR/M byte after the opcode bytes sequence.

      --  General-purpose register (GPR) in the "reg" field of the ModR/M byte

      C_Gd,
      C_Gw,
      C_Gz,
      C_Gb,
      C_Gv,
      C_Gy,

      --  Either a GPR or a memory address in the "r/m" field of the ModR/M
      --  byte.

      C_Eb,
      C_Ed,
      C_Ep,
      C_Ev,
      C_Ew,
      C_Ey,

      --  Control, Debug and general Registers in the "reg" field of the ModR/M
      --  byte.

      C_Cd,
      C_Dd,
      C_Rd, --  The "mod" field may refer only to a GPR
      C_Rv,

      --  Packed quadword MMX register in the "R/M" field of the ModR/M byte

      C_Nq,

      --  Packed quadword MMX register in the "reg" field of the ModR/M byte

      C_Pd,
      C_Pq,
      C_Pw,

      --  Packed quadword MMX register or a memory address

      C_Qd,
      C_Qdq,
      C_Qq,

      --  128-bit XMM register in the "reg" field of the ModR/M byte

      C_Vd,
      C_Vdq,
      C_Vps,
      C_Vpd,
      C_Vq,
      C_Vs,
      C_Vsd,
      C_Vss,
      C_Vw,
      C_Vy,
      C_Vx,

      --  128-bit XMM register or a memory address

      C_Wdq,
      C_Wps,
      C_Wpd,
      C_Wq,
      C_Wsd,
      C_Wss,
      C_Wx,

      --  Coprocessor stack register

      C_H0, --  st(0)
      C_H,  --  st(X)

      --  Memory reference to a floating point value

      C_Mfs, --  Single-precision (32 bits)
      C_Mfd, --  Double-precision (64 bits)
      C_Mfe, --  Extended-precision (80 bits)

      C_Sw, --  Segment register in the "reg" field of the ModR/M byte

      C_Ux, --  XMM register in the "R/M" field of the ModR/M byte

      --  Memory-only or register operand (some SSE instructions have the same
      --  encoding).

      C_Mq_Uq,

      --  Only memory operand ("mod" field of the ModR/M byte != 2#11#)

      C_Ma,
      C_Mp,
      C_Md,
      C_Mb,
      C_Mw,
      C_Mpd,
      C_Mps,
      C_Mdq,
      C_Mq,
      C_Ms,
      C_Msd,
      C_Mss,
      C_M,

      ------------------
      -- End of Modrm --
      ------------------

      --  8-bit specific register

      C_Reg_Al,
      C_Reg_Cl,
      C_Reg_Dl,
      C_Reg_Bl,
      C_Reg_Ch,
      C_Reg_Dh,
      C_Reg_Bh,
      C_Reg_Ah,

      --  16-bit, 32-bit or 64-bit register, depending on the operand-size
      --  attribute and on the presence of a REX prefix.

      C_Reg_Ax,
      C_Reg_Cx,
      C_Reg_Bx,
      C_Reg_Dx,
      C_Reg_Sp,
      C_Reg_Bp,
      C_Reg_Si,
      C_Reg_Di,

      --  Segment register

      C_Reg_Cs,
      C_Reg_Ds,
      C_Reg_Es,
      C_Reg_Fs,
      C_Reg_Gs,
      C_Reg_Ss,

      --  Predefined set of registers.  Used by the MONITOR instruction.

      C_Regs_Eax_Ecx_Edx,

      --  Immediate operand

      C_Iv,
      C_Ib,
      C_Iz,
      C_Iw,

      --  Relative offset to be added to the instruction pointer register to
      --  get the operand value.

      C_Jb,
      C_Jz,

      --  Memory addressed by the DS:SI register pair

      C_Xb,
      C_Xz,
      C_Xv,

      --  Memory addressed by the ES:DI register pair

      C_Yb,
      C_Yz,
      C_Yv,

      C_Ap, --  Immediate far jump/call destination address
      C_Fv, --  EFLAGS register

      --  Immediate operand address
      C_Ob,
      C_Ov,

      C_Cst_1 -- Immediate 1 operand
     );

   subtype Modrm_Code is Code_Type range C_Eb .. C_M;
   --  Subset of operand types that imply a ModR/M byte after the opcode bytes
   --  sequence.

   subtype GPR_Code is Code_Type range C_Reg_Al .. C_Reg_Di;
   --  Subset of operand type that design a specific register

   subtype Prefix_Seg_Code is Code_Type range C_Prefix_Cs .. C_Prefix_Gs;
   --  Subset of operand type that design a segment-override prefix

   type Extra_Operand_Type is
     (Extra_None, Extra_8, Extra_Iz, Extra_Cl);

   -----------------------------
   -- Opcodes decoding tables --
   -----------------------------

   subtype String16 is String (1 .. 16);
   --  Instructions and groups labels, right-padded with spaces

   type Insn_Desc_Type is record
      Name : String16;
      --  Name of the operation

      Dst, Src : Code_Type;
      --  Destination and source operands (C_None if absent)
      Extra : Extra_Operand_Type;
      --  Kind of the last operand if there is an immediate/CL register *and*
      --  destination and source operands, Extra_None otherwise.
   end record;
   --  Format description for one instruction or one instruction group

   Invalid_Desc : constant Insn_Desc_Type :=
     ("invalid*        ", C_None, C_None, Extra_None);

   --  TODO: fix tables for 64-bit mode, keeping the 32-bit compatibility

   type Insn_Desc_Array_Type is array (Byte) of Insn_Desc_Type;
   --  Lookup table kind for one-byte and two-bytes opcode sequences

   type Group_Desc_Array_Type is array (Bit_Field_3) of Insn_Desc_Type;
   type Sub_Group_Desc_Array_Type is array (Bit_Field_3) of Insn_Desc_Type;
   --  Lookup table kind for ModR/M-extended opcodes (on the "register" field
   --  and on the "R/M" byte).

   --  Lookup table for the first byte of opcode sequences

   Insn_Desc : constant Insn_Desc_Array_Type :=
     (
      --  00-07
      2#00_000_000# => ("add             ", C_Eb, C_Gb, Extra_None),
      2#00_000_001# => ("add             ", C_Ev, C_Gv, Extra_None),
      2#00_000_010# => ("add             ", C_Gb, C_Eb, Extra_None),
      2#00_000_011# => ("add             ", C_Gv, C_Ev, Extra_None),
      2#00_000_100# => ("add             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_000_101# => ("add             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_000_110# => ("push            ", C_Reg_Es, C_None, Extra_None),
      2#00_000_111# => ("pop             ", C_Reg_Es, C_None, Extra_None),

      --  08-0F
      2#00_001_000# => ("or              ", C_Eb, C_Gb, Extra_None),
      2#00_001_001# => ("or              ", C_Ev, C_Gv, Extra_None),
      2#00_001_010# => ("or              ", C_Gb, C_Eb, Extra_None),
      2#00_001_011# => ("or              ", C_Gv, C_Ev, Extra_None),
      2#00_001_100# => ("or              ", C_Reg_Al, C_Ib, Extra_None),
      2#00_001_101# => ("or              ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_001_110# => ("push            ", C_Reg_Cs, C_None, Extra_None),
      2#00_001_111# => ("-               ", C_0F, C_None, Extra_None),

      --  10-17
      2#00_010_000# => ("adc             ", C_Eb, C_Gb, Extra_None),
      2#00_010_001# => ("adc             ", C_Ev, C_Gv, Extra_None),
      2#00_010_010# => ("adc             ", C_Gb, C_Eb, Extra_None),
      2#00_010_011# => ("adc             ", C_Gv, C_Ev, Extra_None),
      2#00_010_100# => ("adc             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_010_101# => ("adc             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_010_110# => ("push            ", C_Reg_Ss, C_None, Extra_None),
      2#00_010_111# => ("pop             ", C_Reg_Ss, C_None, Extra_None),

      --  18-1F
      2#00_011_000# => ("sbb             ", C_Eb, C_Gb, Extra_None),
      2#00_011_001# => ("sbb             ", C_Ev, C_Gv, Extra_None),
      2#00_011_010# => ("sbb             ", C_Gb, C_Eb, Extra_None),
      2#00_011_011# => ("sbb             ", C_Gv, C_Ev, Extra_None),
      2#00_011_100# => ("sbb             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_011_101# => ("sbb             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_011_110# => ("push            ", C_Reg_Ds, C_None, Extra_None),
      2#00_011_111# => ("pop             ", C_Reg_Ds, C_None, Extra_None),

      --  20-27
      2#00_100_000# => ("and             ", C_Eb, C_Gb, Extra_None),
      2#00_100_001# => ("and             ", C_Ev, C_Gv, Extra_None),
      2#00_100_010# => ("and             ", C_Gb, C_Eb, Extra_None),
      2#00_100_011# => ("and             ", C_Gv, C_Ev, Extra_None),
      2#00_100_100# => ("and             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_100_101# => ("and             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_100_110# => ("                ", C_Prefix_Es, C_None, Extra_None),
      2#00_100_111# => ("daa             ", C_None, C_None, Extra_None),

      --  28-2F
      2#00_101_000# => ("sub             ", C_Eb, C_Gb, Extra_None),
      2#00_101_001# => ("sub             ", C_Ev, C_Gv, Extra_None),
      2#00_101_010# => ("sub             ", C_Gb, C_Eb, Extra_None),
      2#00_101_011# => ("sub             ", C_Gv, C_Ev, Extra_None),
      2#00_101_100# => ("sub             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_101_101# => ("sub             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_101_110# => ("                ", C_Prefix_Cs, C_None, Extra_None),
      2#00_101_111# => ("das             ", C_None, C_None, Extra_None),

      --  30-37
      2#00_110_000# => ("xor             ", C_Eb, C_Gb, Extra_None),
      2#00_110_001# => ("xor             ", C_Ev, C_Gv, Extra_None),
      2#00_110_010# => ("xor             ", C_Gb, C_Eb, Extra_None),
      2#00_110_011# => ("xor             ", C_Gv, C_Ev, Extra_None),
      2#00_110_100# => ("xor             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_110_101# => ("xor             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_110_110# => ("                ", C_Prefix_Ss, C_None, Extra_None),
      2#00_110_111# => ("aaa             ", C_None, C_None, Extra_None),

      --  28-2F
      2#00_111_000# => ("cmp             ", C_Eb, C_Gb, Extra_None),
      2#00_111_001# => ("cmp             ", C_Ev, C_Gv, Extra_None),
      2#00_111_010# => ("cmp             ", C_Gb, C_Eb, Extra_None),
      2#00_111_011# => ("cmp             ", C_Gv, C_Ev, Extra_None),
      2#00_111_100# => ("cmp             ", C_Reg_Al, C_Ib, Extra_None),
      2#00_111_101# => ("cmp             ", C_Reg_Ax, C_Iz, Extra_None),

      2#00_111_110# => ("                ", C_Prefix_Ds, C_None, Extra_None),
      2#00_111_111# => ("aas             ", C_None, C_None, Extra_None),

      --  40-4F
      16#40#        => ("inc             ", C_Reg_Ax, C_None, Extra_None),
      16#41#        => ("inc             ", C_Reg_Cx, C_None, Extra_None),
      16#42#        => ("inc             ", C_Reg_Dx, C_None, Extra_None),
      16#43#        => ("inc             ", C_Reg_Bx, C_None, Extra_None),
      16#44#        => ("inc             ", C_Reg_Sp, C_None, Extra_None),
      16#45#        => ("inc             ", C_Reg_Bp, C_None, Extra_None),
      16#46#        => ("inc             ", C_Reg_Si, C_None, Extra_None),
      16#47#        => ("inc             ", C_Reg_Di, C_None, Extra_None),

      16#48#        => ("dec             ", C_Reg_Ax, C_None, Extra_None),
      16#49#        => ("dec             ", C_Reg_Cx, C_None, Extra_None),
      16#4a#        => ("dec             ", C_Reg_Dx, C_None, Extra_None),
      16#4b#        => ("dec             ", C_Reg_Bx, C_None, Extra_None),
      16#4c#        => ("dec             ", C_Reg_Sp, C_None, Extra_None),
      16#4d#        => ("dec             ", C_Reg_Bp, C_None, Extra_None),
      16#4e#        => ("dec             ", C_Reg_Si, C_None, Extra_None),
      16#4f#        => ("dec             ", C_Reg_Di, C_None, Extra_None),

      --  50-5F
      16#50#        => ("push            ", C_Reg_Ax, C_None, Extra_None),
      16#51#        => ("push            ", C_Reg_Cx, C_None, Extra_None),
      16#52#        => ("push            ", C_Reg_Dx, C_None, Extra_None),
      16#53#        => ("push            ", C_Reg_Bx, C_None, Extra_None),
      16#54#        => ("push            ", C_Reg_Sp, C_None, Extra_None),
      16#55#        => ("push            ", C_Reg_Bp, C_None, Extra_None),
      16#56#        => ("push            ", C_Reg_Si, C_None, Extra_None),
      16#57#        => ("push            ", C_Reg_Di, C_None, Extra_None),

      16#58#        => ("pop             ", C_Reg_Ax, C_None, Extra_None),
      16#59#        => ("pop             ", C_Reg_Cx, C_None, Extra_None),
      16#5a#        => ("pop             ", C_Reg_Dx, C_None, Extra_None),
      16#5b#        => ("pop             ", C_Reg_Bx, C_None, Extra_None),
      16#5c#        => ("pop             ", C_Reg_Sp, C_None, Extra_None),
      16#5d#        => ("pop             ", C_Reg_Bp, C_None, Extra_None),
      16#5e#        => ("pop             ", C_Reg_Si, C_None, Extra_None),
      16#5f#        => ("pop             ", C_Reg_Di, C_None, Extra_None),

      --  60-6F
      16#60#        => ("pusha           ", C_None, C_None, Extra_None),
      16#61#        => ("popa            ", C_None, C_None, Extra_None),
      16#62#        => ("bound           ", C_Gv, C_Ma, Extra_None),
      16#63#        => ("arpl            ", C_Ew, C_Gw, Extra_None),
      16#64#        => ("                ", C_Prefix_Fs, C_None, Extra_None),
      16#65#        => ("                ", C_Prefix_Gs, C_None, Extra_None),
      16#66#        => ("oper            ", C_Prefix_Oper, C_None, Extra_None),
      16#67#        => ("addr            ", C_Prefix_Addr, C_None, Extra_None),

      16#68#        => ("push            ", C_Iz, C_None, Extra_None),
      16#69#        => ("imul            ", C_Gv, C_Ev, Extra_Iz),
      16#6a#        => ("push            ", C_Ib, C_None, Extra_None),
      16#6b#        => ("imul            ", C_Gv, C_Ev, Extra_8),
      16#6c#        => ("ins             ", C_Yb, C_Reg_Dx, Extra_None),
      16#6d#        => ("ins             ", C_Yz, C_Reg_Dx, Extra_None),
      16#6e#        => ("outs            ", C_Reg_Dx, C_Xb, Extra_None),
      16#6f#        => ("outs            ", C_Reg_Dx, C_Xz, Extra_None),

      --  70-7F
      2#0111_0000#  => ("jo              ", C_Jb, C_None, Extra_None),
      2#0111_0001#  => ("jno             ", C_Jb, C_None, Extra_None),
      2#0111_0010#  => ("jb              ", C_Jb, C_None, Extra_None),
      2#0111_0011#  => ("jae             ", C_Jb, C_None, Extra_None),
      2#0111_0100#  => ("je              ", C_Jb, C_None, Extra_None),
      2#0111_0101#  => ("jne             ", C_Jb, C_None, Extra_None),
      2#0111_0110#  => ("jbe             ", C_Jb, C_None, Extra_None),
      2#0111_0111#  => ("ja              ", C_Jb, C_None, Extra_None),
      2#0111_1000#  => ("js              ", C_Jb, C_None, Extra_None),
      2#0111_1001#  => ("jns             ", C_Jb, C_None, Extra_None),
      2#0111_1010#  => ("jp              ", C_Jb, C_None, Extra_None),
      2#0111_1011#  => ("jnp             ", C_Jb, C_None, Extra_None),
      2#0111_1100#  => ("jl              ", C_Jb, C_None, Extra_None),
      2#0111_1101#  => ("jge             ", C_Jb, C_None, Extra_None),
      2#0111_1110#  => ("jle             ", C_Jb, C_None, Extra_None),
      2#0111_1111#  => ("jg              ", C_Jb, C_None, Extra_None),

      --  80-8F
      2#1000_0000#  => ("1               ", C_Eb, C_Ib, Extra_None),
      2#1000_0001#  => ("1               ", C_Ev, C_Iz, Extra_None),
      2#1000_0010#  => ("1               ", C_Eb, C_Ib, Extra_None),
      2#1000_0011#  => ("1               ", C_Ev, C_Ib, Extra_None),

      2#1000_0100#  => ("test            ", C_Eb, C_Gb, Extra_None),
      2#1000_0101#  => ("test            ", C_Ev, C_Gv, Extra_None),
      2#1000_0110#  => ("xchg            ", C_Eb, C_Gb, Extra_None),
      2#1000_0111#  => ("xchg            ", C_Eb, C_Gb, Extra_None),

      2#1000_1000#  => ("mov             ", C_Eb, C_Gb, Extra_None),
      2#1000_1001#  => ("mov             ", C_Ev, C_Gv, Extra_None),
      2#1000_1010#  => ("mov             ", C_Gb, C_Eb, Extra_None),
      2#1000_1011#  => ("mov             ", C_Gv, C_Ev, Extra_None),
      2#1000_1100#  => ("mov             ", C_Ev, C_Sw, Extra_None),
      2#1000_1101#  => ("lea             ", C_Gv, C_M, Extra_None),
      2#1000_1110#  => ("mov             ", C_Sw, C_Ew, Extra_None),
      2#1000_1111#  => ("pop             ", C_Ev, C_None, Extra_None),

      --  90-9F
      2#1001_0000#  => ("nop             ", C_None, C_None, Extra_None),
      16#91#        => ("xchg            ", C_Reg_Ax, C_Reg_Cx, Extra_None),
      16#92#        => ("xchg            ", C_Reg_Ax, C_Reg_Dx, Extra_None),
      16#93#        => ("xchg            ", C_Reg_Ax, C_Reg_Bx, Extra_None),
      16#94#        => ("xchg            ", C_Reg_Ax, C_Reg_Sp, Extra_None),
      16#95#        => ("xchg            ", C_Reg_Ax, C_Reg_Bp, Extra_None),
      16#96#        => ("xchg            ", C_Reg_Ax, C_Reg_Si, Extra_None),
      16#97#        => ("xchg            ", C_Reg_Ax, C_Reg_Di, Extra_None),

      16#98#        => ("cbw             ", C_None, C_None, Extra_None),
      16#99#        => ("cwd             ", C_None, C_None, Extra_None),
      16#9a#        => ("callf           ", C_Ap, C_None, Extra_None),
      16#9b#        => ("fwait           ", C_None, C_None, Extra_None),
      16#9c#        => ("pushf           ", C_Fv, C_None, Extra_None),
      16#9d#        => ("popf            ", C_Fv, C_None, Extra_None),
      16#9e#        => ("sahf            ", C_None, C_None, Extra_None),
      16#9f#        => ("lahf            ", C_None, C_None, Extra_None),

      --  A0-AF
      16#a0#        => ("mov             ", C_Reg_Al, C_Ob, Extra_None),
      16#a1#        => ("mov             ", C_Reg_Ax, C_Ov, Extra_None),
      16#a2#        => ("mov             ", C_Ob, C_Reg_Al, Extra_None),
      16#a3#        => ("mov             ", C_Ov, C_Reg_Ax, Extra_None),

      16#a4#        => ("movs            ", C_Xb, C_Yb, Extra_None),
      16#a5#        => ("movs            ", C_Xv, C_Yv, Extra_None),
      16#a6#        => ("cmps            ", C_Xb, C_Yb, Extra_None),
      16#a7#        => ("cmps            ", C_Xv, C_Yv, Extra_None),

      16#a8#        => ("test            ", C_Reg_Al, C_Ib, Extra_None),
      16#a9#        => ("test            ", C_Reg_Ax, C_Iz, Extra_None),
      16#aa#        => ("stos            ", C_Yb, C_Reg_Al, Extra_None),
      16#ab#        => ("stos            ", C_Yv, C_Reg_Ax, Extra_None),
      16#ac#        => ("lods            ", C_Reg_Al, C_Xb, Extra_None),
      16#ad#        => ("lods            ", C_Reg_Ax, C_Xv, Extra_None),
      --  FIXME: Xb or Yb?
      16#ae#        => ("scas            ", C_Reg_Al, C_Xb, Extra_None),
      16#af#        => ("scas            ", C_Reg_Ax, C_Xv, Extra_None),

      --  B0-BF
      16#b0#        => ("mov             ", C_Reg_Al, C_Ib, Extra_None),
      16#b1#        => ("mov             ", C_Reg_Cl, C_Ib, Extra_None),
      16#b2#        => ("mov             ", C_Reg_Dl, C_Ib, Extra_None),
      16#b3#        => ("mov             ", C_Reg_Bl, C_Ib, Extra_None),
      16#b4#        => ("mov             ", C_Reg_Ah, C_Ib, Extra_None),
      16#b5#        => ("mov             ", C_Reg_Ch, C_Ib, Extra_None),
      16#b6#        => ("mov             ", C_Reg_Dh, C_Ib, Extra_None),
      16#b7#        => ("mov             ", C_Reg_Bh, C_Ib, Extra_None),
      16#b8#        => ("mov             ", C_Reg_Ax, C_Iv, Extra_None),
      16#b9#        => ("mov             ", C_Reg_Cx, C_Iv, Extra_None),
      16#ba#        => ("mov             ", C_Reg_Dx, C_Iv, Extra_None),
      16#bb#        => ("mov             ", C_Reg_Bx, C_Iv, Extra_None),
      16#bc#        => ("mov             ", C_Reg_Sp, C_Iv, Extra_None),
      16#bd#        => ("mov             ", C_Reg_Bp, C_Iv, Extra_None),
      16#be#        => ("mov             ", C_Reg_Si, C_Iv, Extra_None),
      16#bf#        => ("mov             ", C_Reg_Di, C_Iv, Extra_None),

      --  C0-CF
      16#c0#        => ("2               ", C_Eb, C_Ib, Extra_None),
      16#c1#        => ("2               ", C_Ev, C_Ib, Extra_None),

      16#c2#        => ("ret             ", C_Iw, C_None, Extra_None),
      16#c3#        => ("ret             ", C_None, C_None, Extra_None),
      16#c4#        => ("les             ", C_Gz, C_Mp, Extra_None),
      16#c5#        => ("lds             ", C_Gz, C_Mp, Extra_None),
      16#c6#        => ("mov             ", C_Eb, C_Ib, Extra_None),
      16#c7#        => ("mov             ", C_Ev, C_Iz, Extra_None),

      16#c8#        => ("enter           ", C_Iw, C_Ib, Extra_None),
      16#c9#        => ("leave           ", C_None, C_None, Extra_None),
      16#ca#        => ("retf            ", C_Iw, C_None, Extra_None),
      16#cb#        => ("retf            ", C_None, C_None, Extra_None),
      16#cc#        => ("int3            ", C_None, C_None, Extra_None),
      16#cd#        => ("int             ", C_Ib, C_None, Extra_None),
      16#ce#        => ("into            ", C_None, C_None, Extra_None),
      16#cf#        => ("iret            ", C_None, C_None, Extra_None),

      --  D0-DF
      16#d0#        => ("2               ", C_Eb, C_Cst_1, Extra_None),
      16#d1#        => ("2               ", C_Ev, C_Cst_1, Extra_None),
      16#d2#        => ("2               ", C_Eb, C_Reg_Cl, Extra_None),
      16#d3#        => ("2               ", C_Ev, C_Reg_Cl, Extra_None),
      16#d4#        => ("aam             ", C_Ib, C_None, Extra_None),
      16#d5#        => ("aad             ", C_Ib, C_None, Extra_None),
      16#d6#        => ("                ", C_None, C_None, Extra_None),
      16#d7#        => ("xlat            ", C_None, C_None, Extra_None),
      16#d8#        => ("ESC             ", C_M, C_None, Extra_None),
      16#d9#        => ("ESC             ", C_M, C_None, Extra_None),
      16#da#        => ("ESC             ", C_M, C_None, Extra_None),
      16#db#        => ("ESC             ", C_M, C_None, Extra_None),
      16#dc#        => ("ESC             ", C_M, C_None, Extra_None),
      16#dd#        => ("ESC             ", C_M, C_None, Extra_None),
      16#de#        => ("ESC             ", C_M, C_None, Extra_None),
      16#df#        => ("ESC             ", C_M, C_None, Extra_None),

      --  E0-EF
      16#e0#        => ("loopne          ", C_Jb, C_None, Extra_None),
      16#e1#        => ("loope           ", C_Jb, C_None, Extra_None),
      16#e2#        => ("loop            ", C_Jb, C_None, Extra_None),
      16#e3#        => ("jrcxz           ", C_Jb, C_None, Extra_None),
      16#e4#        => ("in              ", C_Reg_Al, C_Ib, Extra_None),
      16#e5#        => ("in              ", C_Reg_Ax, C_Ib, Extra_None),
      16#e6#        => ("out             ", C_Ib, C_Reg_Al, Extra_None),
      16#e7#        => ("out             ", C_Ib, C_Reg_Ax, Extra_None),

      16#e8#        => ("call            ", C_Jz, C_None, Extra_None),
      16#e9#        => ("jmp             ", C_Jz, C_None, Extra_None),
      16#ea#        => ("jmpf            ", C_Ap, C_None, Extra_None),
      16#eb#        => ("jmp             ", C_Jb, C_None, Extra_None),
      16#ec#        => ("in              ", C_Reg_Al, C_Reg_Dx, Extra_None),
      16#ed#        => ("in              ", C_Reg_Ax, C_Reg_Dx, Extra_None),
      16#ee#        => ("out             ", C_Reg_Dx, C_Reg_Al, Extra_None),
      16#ef#        => ("out             ", C_Reg_Dx, C_Reg_Ax, Extra_None),

      --  F0-FF
      16#f0#        => ("lock            ", C_Prefix_Lock, C_None, Extra_None),
      16#f1#        => ("                ", C_None, C_None, Extra_None),
      16#f2#        =>
        ("repne           ", C_Prefix_Repne, C_None, Extra_None),
      16#f3#        => ("rep             ", C_Prefix_Rep, C_None, Extra_None),
      16#f4#        => ("hlt             ", C_None, C_None, Extra_None),
      16#f5#        => ("cmc             ", C_None, C_None, Extra_None),
      16#f6#        => ("3               ", C_Eb, C_None, Extra_None),
      16#f7#        => ("3               ", C_Ev, C_None, Extra_None),
      16#f8#        => ("clc             ", C_None, C_None, Extra_None),
      16#f9#        => ("stc             ", C_None, C_None, Extra_None),
      16#fa#        => ("cli             ", C_None, C_None, Extra_None),
      16#fb#        => ("sti             ", C_None, C_None, Extra_None),
      16#fc#        => ("cld             ", C_None, C_None, Extra_None),
      16#fd#        => ("std             ", C_None, C_None, Extra_None),
      16#fe#        => ("4               ", C_None, C_None, Extra_None),
      16#ff#        => ("5               ", C_None, C_None, Extra_None));

   --  Lookup table for two-bytes opcode sequences with no mandatory prefix.
   --  The first byte of the opcode sequence is 16#0f# and this table is
   --  indexed by its second byte.

   Insn_Desc_0F : constant Insn_Desc_Array_Type :=
     (
      16#00#        => ("6               ", C_None, C_None, Extra_None),
      16#01#        => ("7               ", C_None, C_None, Extra_None),
      16#02#        => ("lar             ", C_Gv, C_Ew, Extra_None),
      16#03#        => ("lsl             ", C_Gv, C_Ew, Extra_None),
      16#04#        => ("                ", C_None, C_None, Extra_None),
      16#05#        => ("syscall         ", C_None, C_None, Extra_None),
      16#06#        => ("clts            ", C_None, C_None, Extra_None),
      16#07#        => ("sysret          ", C_None, C_None, Extra_None),
      16#08#        => ("invd            ", C_None, C_None, Extra_None),
      16#09#        => ("wbinvd          ", C_None, C_None, Extra_None),
      16#0a#        => ("                ", C_None, C_None, Extra_None),
      16#0b#        => ("ud2             ", C_None, C_None, Extra_None),
      16#0c#        => ("                ", C_None, C_None, Extra_None),
      16#0d#        => ("nop             ", C_Ev, C_None, Extra_None),
      16#0e#        => ("                ", C_None, C_None, Extra_None),
      16#0f#        => ("                ", C_None, C_None, Extra_None),

      16#10#        => ("movups          ", C_Vps, C_Wps, Extra_None),
      16#11#        => ("movups          ", C_Wps, C_Vps, Extra_None),
      16#12#        => ("movlps          ", C_Vq, C_Mq_Uq, Extra_None),
      16#13#        => ("movlps          ", C_Mq, C_Vq, Extra_None),
      16#14#        => ("unpcklps        ", C_Vs, C_Wps, Extra_None),
      16#15#        => ("unpckhps        ", C_Vs, C_Wps, Extra_None),
      16#16#        => ("movhps          ", C_Vq, C_Mq_Uq, Extra_None),
      16#17#        => ("movhps          ", C_Mq, C_Vps, Extra_None),
      16#18#        => ("16              ", C_None, C_None, Extra_None),
      16#1f#        => ("nop             ", C_Ev, C_None, Extra_None),

      16#20#        => ("mov             ", C_Rd, C_Cd, Extra_None),
      16#21#        => ("mov             ", C_Rd, C_Dd, Extra_None),
      16#22#        => ("mov             ", C_Cd, C_Rd, Extra_None),
      16#23#        => ("mov             ", C_Dd, C_Rd, Extra_None),
      --  The 16#25# slot is reserved
      --  The 16#24# and 16#26# slots is a MOV for test registers. Not
      --  documented.
      --  The 16#27# slot is reserved
      16#28#        => ("movaps          ", C_Vps, C_Wps, Extra_None),
      16#29#        => ("movaps          ", C_Wps, C_Vps, Extra_None),
      16#2a#        => ("cvtpi2ps        ", C_Vps, C_Qq, Extra_None),
      16#2b#        => ("movntps         ", C_Mps, C_Vps, Extra_None),
      16#2c#        => ("cvttps2pi       ", C_Pq,  C_Wps, Extra_None),
      16#2d#        => ("cvtps2pi        ", C_Pq,  C_Wps, Extra_None),
      16#2e#        => ("ucomiss         ", C_Vss, C_Wss, Extra_None),
      16#2f#        => ("comiss          ", C_Vss, C_Wss, Extra_None),

      16#30#        => ("wrmsr           ", C_None, C_None, Extra_None),
      16#31#        => ("rdtsc           ", C_None, C_None, Extra_None),
      16#32#        => ("rdmsr           ", C_None, C_None, Extra_None),
      16#33#        => ("rdpmc           ", C_None, C_None, Extra_None),
      16#34#        => ("sysenter        ", C_None, C_None, Extra_None),
      16#35#        => ("sysexit         ", C_None, C_None, Extra_None),
      16#37#        => ("getsec          ", C_None, C_None, Extra_None),
      --  The 16#36# and 16#38-16#3f# slots are reserved

      16#40#        => ("cmovo           ", C_Gv, C_Ev, Extra_None),
      16#41#        => ("cmovno          ", C_Gv, C_Ev, Extra_None),
      16#42#        => ("cmovb           ", C_Gv, C_Ev, Extra_None),
      16#43#        => ("cmovae          ", C_Gv, C_Ev, Extra_None),
      16#44#        => ("cmove           ", C_Gv, C_Ev, Extra_None),
      16#45#        => ("cmovne          ", C_Gv, C_Ev, Extra_None),
      16#46#        => ("cmovbe          ", C_Gv, C_Ev, Extra_None),
      16#47#        => ("cmova           ", C_Gv, C_Ev, Extra_None),
      16#48#        => ("cmovs           ", C_Gv, C_Ev, Extra_None),
      16#49#        => ("cmovns          ", C_Gv, C_Ev, Extra_None),
      16#4a#        => ("cmovpe          ", C_Gv, C_Ev, Extra_None),
      16#4b#        => ("cmovpo          ", C_Gv, C_Ev, Extra_None),
      16#4c#        => ("cmovl           ", C_Gv, C_Ev, Extra_None),
      16#4d#        => ("cmovge          ", C_Gv, C_Ev, Extra_None),
      16#4e#        => ("cmovle          ", C_Gv, C_Ev, Extra_None),
      16#4f#        => ("cmovg           ", C_Gv, C_Ev, Extra_None),

      16#50#        => ("movmskps        ", C_Gd, C_Vps, Extra_None),
      16#51#        => ("sqrtps          ", C_Vps, C_Wps, Extra_None),
      16#52#        => ("rsqrtps         ", C_Vps, C_Wps, Extra_None),
      16#53#        => ("rcpps           ", C_Vps, C_Wps, Extra_None),
      16#54#        => ("andps           ", C_Vps, C_Wps, Extra_None),
      16#55#        => ("andnps          ", C_Vps, C_Wps, Extra_None),
      16#56#        => ("orps            ", C_Vps, C_Wps, Extra_None),
      16#57#        => ("xorps           ", C_Vps, C_Wps, Extra_None),
      16#58#        => ("addps           ", C_Vps, C_Wps, Extra_None),
      16#59#        => ("mulps           ", C_Vps, C_Wps, Extra_None),
      16#5a#        => ("cvtps2pd        ", C_Vps, C_Wps, Extra_None),
      16#5b#        => ("cvtdq2ps        ", C_Vps, C_Wps, Extra_None),
      16#5c#        => ("subps           ", C_Vps, C_Wps, Extra_None),
      16#5d#        => ("minps           ", C_Vps, C_Wps, Extra_None),
      16#5e#        => ("divps           ", C_Vps, C_Wps, Extra_None),
      16#5f#        => ("maxps           ", C_Vps, C_Wps, Extra_None),

      16#60#        => ("punpcklbw       ", C_Pq, C_Qd, Extra_None),
      16#61#        => ("punpcklwd       ", C_Pq, C_Qd, Extra_None),
      16#62#        => ("punpckldq       ", C_Pq, C_Qd, Extra_None),
      16#63#        => ("packsswb        ", C_Pq, C_Qq, Extra_None),
      16#64#        => ("pcmpgtb         ", C_Pq, C_Qq, Extra_None),
      16#65#        => ("pcmpgtw         ", C_Pq, C_Qq, Extra_None),
      16#66#        => ("pcmpgtd         ", C_Pq, C_Qq, Extra_None),
      16#67#        => ("packuswb        ", C_Pq, C_Qq, Extra_None),
      16#68#        => ("punpckhbw       ", C_Pq, C_Qq, Extra_None),
      16#69#        => ("punpckhwd       ", C_Pq, C_Qq, Extra_None),
      16#6a#        => ("punpckhdq       ", C_Pq, C_Qq, Extra_None),
      16#6b#        => ("packssdw        ", C_Pq, C_Qq, Extra_None),
      --  The 16#6c# and 16#6d# slots are reserved
      16#6e#        => ("movd            ", C_Pq, C_Ed, Extra_None),
      16#6f#        => ("movq            ", C_Pq, C_Qq, Extra_None),

      16#70#        => ("pshufw          ", C_Pq, C_Qq, Extra_8),
      16#71#        => ("12              ", C_None, C_None, Extra_None),
      16#72#        => ("13              ", C_None, C_None, Extra_None),
      16#73#        => ("14              ", C_None, C_None, Extra_None),
      16#74#        => ("pcmpeqb         ", C_Pq, C_Qq, Extra_None),
      16#75#        => ("pcmpeqw         ", C_Pq, C_Qq, Extra_None),
      16#76#        => ("pcmepeqd        ", C_Pq, C_Qq, Extra_None),
      16#77#        => ("emms            ", C_None, C_None, Extra_None),
      16#78#        => ("vmread          ", C_Ey, C_Gy, Extra_None),
      16#79#        => ("vmwrite         ", C_Gy, C_Ey, Extra_None),
      --  The 16#7a#-16#7b# slots are reserved
      16#7c#        => ("haddpd          ", C_Vpd, C_Wpd, Extra_None),
      16#7d#        => ("hsubpd          ", C_Vpd, C_Wpd, Extra_None),
      16#7e#        => ("movd            ", C_Ed, C_Pd, Extra_None),
      16#7f#        => ("movq            ", C_Qq, C_Pq, Extra_None),

      2#1000_0000#  => ("jo              ", C_Jz, C_None, Extra_None),
      2#1000_0001#  => ("jno             ", C_Jz, C_None, Extra_None),
      2#1000_0010#  => ("jb              ", C_Jz, C_None, Extra_None),
      2#1000_0011#  => ("jae             ", C_Jz, C_None, Extra_None),
      2#1000_0100#  => ("je              ", C_Jz, C_None, Extra_None),
      2#1000_0101#  => ("jne             ", C_Jz, C_None, Extra_None),
      2#1000_0110#  => ("jbe             ", C_Jz, C_None, Extra_None),
      2#1000_0111#  => ("ja              ", C_Jz, C_None, Extra_None),
      2#1000_1000#  => ("js              ", C_Jz, C_None, Extra_None),
      2#1000_1001#  => ("jns             ", C_Jz, C_None, Extra_None),
      2#1000_1010#  => ("jp              ", C_Jz, C_None, Extra_None),
      2#1000_1011#  => ("jnp             ", C_Jz, C_None, Extra_None),
      2#1000_1100#  => ("jl              ", C_Jz, C_None, Extra_None),
      2#1000_1101#  => ("jge             ", C_Jz, C_None, Extra_None),
      2#1000_1110#  => ("jle             ", C_Jz, C_None, Extra_None),
      2#1000_1111#  => ("jg              ", C_Jz, C_None, Extra_None),

      2#1001_0000#  => ("seto            ", C_Eb, C_None, Extra_None),
      2#1001_0001#  => ("setno           ", C_Eb, C_None, Extra_None),
      2#1001_0010#  => ("setb            ", C_Eb, C_None, Extra_None),
      2#1001_0011#  => ("setae           ", C_Eb, C_None, Extra_None),
      2#1001_0100#  => ("sete            ", C_Eb, C_None, Extra_None),
      2#1001_0101#  => ("setne           ", C_Eb, C_None, Extra_None),
      2#1001_0110#  => ("setbe           ", C_Eb, C_None, Extra_None),
      2#1001_0111#  => ("seta            ", C_Eb, C_None, Extra_None),
      2#1001_1000#  => ("sets            ", C_Eb, C_None, Extra_None),
      2#1001_1001#  => ("setns           ", C_Eb, C_None, Extra_None),
      2#1001_1010#  => ("setp            ", C_Eb, C_None, Extra_None),
      2#1001_1011#  => ("setnp           ", C_Eb, C_None, Extra_None),
      2#1001_1100#  => ("setl            ", C_Eb, C_None, Extra_None),
      2#1001_1101#  => ("setge           ", C_Eb, C_None, Extra_None),
      2#1001_1110#  => ("setle           ", C_Eb, C_None, Extra_None),
      2#1001_1111#  => ("setjg           ", C_Eb, C_None, Extra_None),

      16#a0#        => ("push            ", C_Reg_Fs, C_None, Extra_None),
      16#a1#        => ("pop             ", C_Reg_Fs, C_None, Extra_None),
      16#a2#        => ("cpuid           ", C_None, C_None, Extra_None),
      16#a3#        => ("bt              ", C_Ev, C_Gv, Extra_None),
      16#a4#        => ("shld            ", C_Ev, C_Gv, Extra_8),
      16#a5#        => ("shld            ", C_Ev, C_Gv, Extra_Cl),
      16#a8#        => ("push            ", C_Reg_Gs, C_None, Extra_None),
      16#a9#        => ("pop             ", C_Reg_Gs, C_None, Extra_None),
      16#aa#        => ("rsm             ", C_None, C_None, Extra_None),
      16#ab#        => ("bts             ", C_Ev, C_Gv, Extra_None),
      16#ac#        => ("shrd            ", C_Ev, C_Gv, Extra_8),
      16#ad#        => ("shrd            ", C_Ev, C_Gv, Extra_Cl),
      16#ae#        => ("15              ", C_None, C_None, Extra_None),
      16#af#        => ("imul            ", C_Gv, C_Ev, Extra_None),

      16#b0#        => ("cmpxchg         ", C_Eb, C_Gb, Extra_None),
      16#b1#        => ("cmpxchg         ", C_Ev, C_Gv, Extra_None),
      16#b2#        => ("lss             ", C_Gv, C_Mp, Extra_None),
      16#b3#        => ("btr             ", C_Ev, C_Gv, Extra_None),
      16#b4#        => ("lfs             ", C_Gv, C_Mp, Extra_None),
      16#b5#        => ("lgs             ", C_Gv, C_Mp, Extra_None),
      16#b6#        => ("movzx           ", C_Gv, C_Eb, Extra_None),
      16#b7#        => ("movzx           ", C_Gv, C_Ew, Extra_None),
      16#b9#        => ("ud2             ", C_None, C_None, Extra_None),
      16#ba#        => ("8               ", C_None, C_None, Extra_None),
      16#bb#        => ("btc             ", C_Ev, C_Gv, Extra_None),
      16#bc#        => ("bsf             ", C_Gv, C_Ev, Extra_None),
      16#bd#        => ("bsr             ", C_Gv, C_Ev, Extra_None),
      16#be#        => ("movsx           ", C_Gv, C_Eb, Extra_None),
      16#bf#        => ("movsx           ", C_Gv, C_Ew, Extra_None),

      16#c0#        => ("xadd            ", C_Eb, C_Gb, Extra_None),
      16#c1#        => ("xadd            ", C_Ev, C_Gv, Extra_None),
      16#c2#        => ("cmpps           ", C_Vps, C_Wps, Extra_8),
      16#c3#        => ("movnti          ", C_Md, C_Gd, Extra_None),
      16#c4#        => ("pinsrw          ", C_Pw, C_Ew, Extra_8),
      16#c5#        => ("pextrw          ", C_Gw, C_Pw, Extra_8),
      16#c6#        => ("shufps          ", C_Vps, C_Wps, Extra_8),
      16#c7#        => ("9               ", C_None, C_None, Extra_None),
      16#c8#        => ("bswap           ", C_Reg_Ax, C_None, Extra_None),
      16#c9#        => ("bswap           ", C_Reg_Cx, C_None, Extra_None),
      16#ca#        => ("bswap           ", C_Reg_Dx, C_None, Extra_None),
      16#cb#        => ("bswap           ", C_Reg_Bx, C_None, Extra_None),
      16#cc#        => ("bswap           ", C_Reg_Sp, C_None, Extra_None),
      16#cd#        => ("bswap           ", C_Reg_Bp, C_None, Extra_None),
      16#ce#        => ("bswap           ", C_Reg_Si, C_None, Extra_None),
      16#cf#        => ("bswap           ", C_Reg_Di, C_None, Extra_None),

      --  The 16#d0# slot is reserved
      16#d1#        => ("psrlw           ", C_Pq, C_Qq, Extra_None),
      16#d2#        => ("psrld           ", C_Pq, C_Qq, Extra_None),
      16#d3#        => ("psrlq           ", C_Pq, C_Qq, Extra_None),
      16#d4#        => ("paddq           ", C_Pq, C_Qq, Extra_None),
      16#d5#        => ("pmullw          ", C_Pq, C_Qq, Extra_None),
      --  The 16#d6# slot is reserved
      16#d7#        => ("pmovmskb        ", C_Gd, C_Nq, Extra_None),
      16#d8#        => ("psubusb         ", C_Pq, C_Qq, Extra_None),
      16#d9#        => ("psubusw         ", C_Pq, C_Qq, Extra_None),
      16#da#        => ("pminub          ", C_Pq, C_Qq, Extra_None),
      16#db#        => ("pand            ", C_Pq, C_Qq, Extra_None),
      16#dc#        => ("paddusb         ", C_Pq, C_Qq, Extra_None),
      16#dd#        => ("paddusw         ", C_Pq, C_Qq, Extra_None),
      16#de#        => ("pmaxub          ", C_Pq, C_Qq, Extra_None),
      16#df#        => ("pandn           ", C_Pq, C_Qq, Extra_None),

      16#e0#        => ("pavgb           ", C_Pq, C_Qq, Extra_None),
      16#e1#        => ("psraw           ", C_Pq, C_Qq, Extra_None),
      16#e2#        => ("psrad           ", C_Pq, C_Qq, Extra_None),
      16#e3#        => ("pavgw           ", C_Pq, C_Qq, Extra_None),
      16#e4#        => ("pmulhuw         ", C_Pq, C_Qq, Extra_None),
      16#e5#        => ("pmulhw          ", C_Pq, C_Qq, Extra_None),
      --  The 16#e6# slot is reserved
      16#e7#        => ("movntq          ", C_Mq, C_Vq, Extra_None),
      16#e8#        => ("psubsb          ", C_Pq, C_Qq, Extra_None),
      16#e9#        => ("psubsw          ", C_Pq, C_Qq, Extra_None),
      16#ea#        => ("pminsw          ", C_Pq, C_Qq, Extra_None),
      16#eb#        => ("por             ", C_Pq, C_Qq, Extra_None),
      16#ec#        => ("paddsb          ", C_Pq, C_Qq, Extra_None),
      16#ed#        => ("paddsw          ", C_Pq, C_Qq, Extra_None),
      16#ee#        => ("pmaxsw          ", C_Pq, C_Qq, Extra_None),
      16#ef#        => ("pxor            ", C_Pq, C_Qq, Extra_None),

      --  The 16#f0# slot is reserved
      16#f1#        => ("psllw           ", C_Pq, C_Qq, Extra_None),
      16#f2#        => ("pslld           ", C_Pq, C_Qq, Extra_None),
      16#f3#        => ("psllq           ", C_Pq, C_Qq, Extra_None),
      16#f4#        => ("pmuludq         ", C_Pq, C_Qq, Extra_None),
      16#f5#        => ("pmaddwd         ", C_Pq, C_Qq, Extra_None),
      16#f6#        => ("psadbw          ", C_Pq, C_Qq, Extra_None),
      16#f7#        => ("maskmovq        ", C_Pq, C_Nq, Extra_None),
      16#f8#        => ("psubb           ", C_Pq, C_Qq, Extra_None),
      16#f9#        => ("psubw           ", C_Pq, C_Qq, Extra_None),
      16#fa#        => ("psubd           ", C_Pq, C_Qq, Extra_None),
      16#fb#        => ("psubq           ", C_Pq, C_Qq, Extra_None),
      16#fc#        => ("paddb           ", C_Pq, C_Qq, Extra_None),
      16#fd#        => ("paddw           ", C_Pq, C_Qq, Extra_None),
      16#fe#        => ("paddd           ", C_Pq, C_Qq, Extra_None),
      --  The 16#ff# slot is reserved

      others       =>  ("                ", C_None, C_None, Extra_None));

   --  Lookup table for two-bytes opcode sequences with the 16#66# mandatory
   --  prefix. The first byte of the opcode sequence is 16#0f# and this table
   --  is indexed by its second byte.

   Insn_Desc_66_0F : constant Insn_Desc_Array_Type :=
     (
      16#10#        => ("movupd          ", C_Pq, C_Qq, Extra_None),
      16#11#        => ("movupd          ", C_Wpd, C_Vpd, Extra_None),
      16#12#        => ("movlpd          ", C_Vq, C_Mq, Extra_None),
      16#13#        => ("movlpd          ", C_Mq, C_Vq, Extra_None),
      16#14#        => ("unpcklpd        ", C_Vpd, C_Wpd, Extra_None),
      16#15#        => ("unpckhpd        ", C_Vpd, C_Wpd, Extra_None),
      16#16#        => ("movhpd          ", C_Vdq, C_Mq, Extra_None),
      16#17#        => ("movhpd          ", C_Mq, C_Vpd, Extra_None),

      16#28#        => ("movapd          ", C_Vpd, C_Wpd, Extra_None),
      16#29#        => ("movapd          ", C_Wpd, C_Vpd, Extra_None),
      16#2a#        => ("cvtpi2pd        ", C_Vpd, C_Qd, Extra_None),
      16#2b#        => ("movntpd         ", C_Mpd, C_Vpd, Extra_None),
      16#2c#        => ("cvttpd2pi       ", C_Pq, C_Wpd, Extra_None),
      16#2d#        => ("cvtpd2pi        ", C_Pq, C_Wpd, Extra_None),
      16#2e#        => ("ucomisd         ", C_Vsd, C_Wsd, Extra_None),
      16#2f#        => ("comisd          ", C_Vsd, C_Wsd, Extra_None),

      16#50#        => ("movmskpd        ", C_Gd, C_Vpd, Extra_None),
      16#51#        => ("sqrtpd          ", C_Vpd, C_Wpd, Extra_None),
      --  The 16#52#-16#63# slots are reserved
      16#54#        => ("andpd           ", C_Vpd, C_Wpd, Extra_None),
      16#55#        => ("andnpd          ", C_Vpd, C_Wpd, Extra_None),
      16#56#        => ("orpdpd          ", C_Vpd, C_Wpd, Extra_None),
      16#57#        => ("xorpd           ", C_Vpd, C_Wpd, Extra_None),
      16#58#        => ("addpd           ", C_Vpd, C_Wpd, Extra_None),
      16#59#        => ("mulpd           ", C_Vpd, C_Wpd, Extra_None),
      16#5a#        => ("cvtp2ps         ", C_Vpd, C_Wpd, Extra_None),
      16#5b#        => ("cvtps2dq        ", C_Vdq, C_Wps, Extra_None),
      16#5c#        => ("subpd           ", C_Vpd, C_Wpd, Extra_None),
      16#5d#        => ("minpd           ", C_Vpd, C_Wpd, Extra_None),
      16#5e#        => ("divpd           ", C_Vpd, C_Wpd, Extra_None),
      16#5f#        => ("maxpd           ", C_Vpd, C_Wpd, Extra_None),

      16#60#        => ("punpcklbw       ", C_Vdq, C_Wdq, Extra_None),
      16#61#        => ("punpcklwd       ", C_Vdq, C_Wdq, Extra_None),
      16#62#        => ("punpckldq       ", C_Vdq, C_Wdq, Extra_None),
      16#63#        => ("packsswb        ", C_Vdq, C_Wdq, Extra_None),
      16#64#        => ("pcmpgtb         ", C_Vdq, C_Wdq, Extra_None),
      16#65#        => ("pcmpgtw         ", C_Vdq, C_Wdq, Extra_None),
      16#66#        => ("pcmpgtd         ", C_Vdq, C_Wdq, Extra_None),
      16#67#        => ("packuswb        ", C_Vdq, C_Wdq, Extra_None),
      16#68#        => ("punpckhbw       ", C_Vdq, C_Qdq, Extra_None),
      16#69#        => ("punpckhwd       ", C_Vdq, C_Qdq, Extra_None),
      16#6a#        => ("punpckhdq       ", C_Vdq, C_Qdq, Extra_None),
      16#6b#        => ("packssdw        ", C_Vdq, C_Qdq, Extra_None),
      16#6c#        => ("punpcklqdq      ", C_Vdq, C_Wdq, Extra_None),
      16#6d#        => ("punpckhqd       ", C_Vdq, C_Wdq, Extra_None),
      16#6e#        => ("movd            ", C_Vd, C_Ed, Extra_None),
      16#6f#        => ("movdqa          ", C_Vdq, C_Wdq, Extra_None),

      16#70#        => ("pshufd          ", C_Vdq, C_Wdq, Extra_8),
      16#71#        => ("12              ", C_None, C_None, Extra_None),
      16#72#        => ("13              ", C_None, C_None, Extra_None),
      16#73#        => ("14              ", C_None, C_None, Extra_None),
      16#74#        => ("pcmpeqb         ", C_Vdq, C_Wdq, Extra_None),
      16#75#        => ("pcmpeqw         ", C_Vdq, C_Wdq, Extra_None),
      16#76#        => ("pcmpeqd         ", C_Vdq, C_Wdq, Extra_None),
      --  The 16#77#-16#7b# slots are reserved
      16#7c#        => ("haddpd          ", C_Vdq, C_Wdq, Extra_None),
      16#7d#        => ("hsubpd          ", C_Vdq, C_Wdq, Extra_None),
      16#7e#        => ("movd            ", C_Ey, C_Vy, Extra_None),
      16#7f#        => ("movdqa          ", C_Wdq, C_Vdq, Extra_None),

      16#c2#        => ("cmpps           ", C_Vpd, C_Wpd, Extra_8),
      16#c4#        => ("pinsrw          ", C_Vw, C_Ew, Extra_8),
      16#c5#        => ("pextrw          ", C_Gw, C_Vw, Extra_8),
      16#c6#        => ("shufpd          ", C_Vpd, C_Wpd, Extra_8),
      --  TODO??? 19 extended opcodes forms

      16#d0#        => ("addsubpd        ", C_Vpd, C_Wpd, Extra_None),
      16#d1#        => ("psrlw           ", C_Vdq, C_Wdq, Extra_None),
      16#d2#        => ("psrld           ", C_Vdq, C_Wdq, Extra_None),
      16#d3#        => ("psrlq           ", C_Vdq, C_Wdq, Extra_None),
      16#d4#        => ("paddq           ", C_Vdq, C_Wdq, Extra_None),
      16#d5#        => ("pmullw          ", C_Vdq, C_Wdq, Extra_None),
      16#d6#        => ("movq            ", C_Wq, C_Vq, Extra_None),
      16#d7#        => ("pmovmskb        ", C_Gd, C_Vdq, Extra_None),
      16#d8#        => ("psubusb         ", C_Vdq, C_Wdq, Extra_None),
      16#d9#        => ("psubusw         ", C_Vdq, C_Wdq, Extra_None),
      16#da#        => ("pminub          ", C_Vdq, C_Wdq, Extra_None),
      16#db#        => ("pand            ", C_Vdq, C_Wdq, Extra_None),
      16#dc#        => ("paddusb         ", C_Vdq, C_Wdq, Extra_None),
      16#dd#        => ("paddusw         ", C_Vdq, C_Wdq, Extra_None),
      16#de#        => ("pmaxub          ", C_Vdq, C_Wdq, Extra_None),
      16#df#        => ("pandn           ", C_Vdq, C_Wdq, Extra_None),

      16#e0#        => ("pavgb           ", C_Vdq, C_Wdq, Extra_None),
      16#e1#        => ("psraw           ", C_Vdq, C_Wdq, Extra_None),
      16#e2#        => ("psrad           ", C_Vdq, C_Wdq, Extra_None),
      16#e3#        => ("pavgw           ", C_Vdq, C_Wdq, Extra_None),
      16#e4#        => ("pmulhuw         ", C_Vdq, C_Wdq, Extra_None),
      16#e5#        => ("pmulhw          ", C_Vdq, C_Wdq, Extra_None),
      16#e6#        => ("cvttpd2dq       ", C_Vdq, C_Wdq, Extra_None),
      16#e7#        => ("movntdq         ", C_Vdq, C_Wdq, Extra_None),
      16#e8#        => ("psubsb          ", C_Vdq, C_Wdq, Extra_None),
      16#e9#        => ("psubsw          ", C_Vdq, C_Wdq, Extra_None),
      16#ea#        => ("pminsw          ", C_Vdq, C_Wdq, Extra_None),
      16#eb#        => ("por             ", C_Vdq, C_Wdq, Extra_None),
      16#ec#        => ("paddsb          ", C_Vdq, C_Wdq, Extra_None),
      16#ed#        => ("paddsw          ", C_Vdq, C_Wdq, Extra_None),
      16#ee#        => ("pmaxsw          ", C_Vdq, C_Wdq, Extra_None),
      16#ef#        => ("pxor            ", C_Vdq, C_Wdq, Extra_None),

      --  The 16#f0# slot is reserved
      16#f1#        => ("psllw           ", C_Vdq, C_Wdq, Extra_None),
      16#f2#        => ("pslld           ", C_Vdq, C_Wdq, Extra_None),
      16#f3#        => ("psllq           ", C_Vdq, C_Wdq, Extra_None),
      16#f4#        => ("pmuludq         ", C_Vdq, C_Wdq, Extra_None),
      16#f5#        => ("pmaddwd         ", C_Vdq, C_Wdq, Extra_None),
      16#f6#        => ("psadbw          ", C_Vdq, C_Wdq, Extra_None),
      16#f7#        => ("maskmovq        ", C_Vdq, C_Wdq, Extra_None),
      16#f8#        => ("psubb           ", C_Vdq, C_Wdq, Extra_None),
      16#f9#        => ("psubw           ", C_Vdq, C_Wdq, Extra_None),
      16#fa#        => ("psubd           ", C_Vdq, C_Wdq, Extra_None),
      16#fb#        => ("psubq           ", C_Vdq, C_Wdq, Extra_None),
      16#fc#        => ("paddb           ", C_Vdq, C_Wdq, Extra_None),
      16#fd#        => ("paddw           ", C_Vdq, C_Wdq, Extra_None),
      16#fe#        => ("paddd           ", C_Vdq, C_Wdq, Extra_None),
      --  The 16#ff# slot is reserved

      others        => ("                ", C_None, C_None, Extra_None));

   --  Lookup table for two-bytes opcode sequences with the 16#f2# mandatory
   --  prefix. The first byte of the opcode sequence is 16#0f# and this table
   --  is indexed by its second byte.

   Insn_Desc_F2_0F : constant Insn_Desc_Array_Type :=
     (
      16#10#        => ("movsd           ", C_Vsd, C_Wsd, Extra_None),
      16#11#        => ("movsd           ", C_Vsd, C_Wsd, Extra_None),
      16#12#        => ("movddup         ", C_Vx, C_Wx, Extra_None),

      16#2a#        => ("cvtsi2sd        ", C_Vsd, C_Ey, Extra_None),
      16#2b#        => ("movntsd         ", C_Msd, C_Vsd, Extra_None),
      16#2c#        => ("cvttsd2si       ", C_Gy, C_Wsd, Extra_None),
      16#2d#        => ("cvtsd2si        ", C_Gy, C_Wsd, Extra_None),

      --  Here...
      16#51#        => ("sqrtsd          ", C_Vsd, C_Wsd, Extra_None),
      --  ... and here, a lot of slots are reserved.
      16#58#        => ("addsd           ", C_Vsd, C_Wsd, Extra_None),
      16#59#        => ("mulsd           ", C_Vsd, C_Wsd, Extra_None),
      16#5a#        => ("cvtsd2ss        ", C_Vsd, C_Wsd, Extra_None),
      --  The 16#5b# slot is reserved
      16#5c#        => ("subsd           ", C_Vsd, C_Wsd, Extra_None),
      16#5d#        => ("minsd           ", C_Vsd, C_Wsd, Extra_None),
      16#5e#        => ("divsd           ", C_Vsd, C_Wsd, Extra_None),
      16#5f#        => ("maxsd           ", C_Vsd, C_Wsd, Extra_None),

      16#70#        => ("pshuflw         ", C_Vdq, C_Wdq, Extra_8),
      --  TODO??? 12/13/14 extended opcodes forms
      --  The 16#74#-16#7b# slots are reserved
      16#7c#        => ("haddps          ", C_Vps, C_Wps, Extra_None),
      16#7d#        => ("hsubps          ", C_Vps, C_Wps, Extra_None),
      --  The 16#7e#-16#7f# slots are reserved

      16#c2#        => ("cmpsd           ", C_Vsd, C_Wsd, Extra_8),
      16#d0#        => ("addsubpd        ", C_Vps, C_Wps, Extra_None),
      16#d6#        => ("movdq2q         ", C_Pq, C_Vq, Extra_None),
      16#e6#        => ("cvtpd2dq        ", C_Vdq, C_Wdq, Extra_None),
      16#f0#        => ("lddqu           ", C_Vdq, C_Mdq, Extra_None),

      others        => ("                ", C_None, C_None, Extra_None));

   --  Lookup table for two-bytes opcode sequences with the 16#f3# mandatory
   --  prefix. The first byte of the opcode sequence is 16#0f# and this table
   --  is indexed by its second byte.

   Insn_Desc_F3_0F : constant Insn_Desc_Array_Type :=
     (
      16#10#        => ("movss           ", C_Vss, C_Wss, Extra_None),
      16#11#        => ("movss           ", C_Wss, C_Vss, Extra_None),
      16#12#        => ("movsldup        ", C_Vx, C_Wx, Extra_None),
      --  The 16#14#-16#15# slots are reserved
      16#16#        => ("movshdup        ", C_Vps, C_Wps, Extra_None),
      --  The 16#17# slot is reserved
      --  TODO??? 16 extended opcodes forms
      --  The 16#19#-16#1f# slots are reserved

      16#2a#        => ("cvtsi2ss        ", C_Vss, C_Ey, Extra_None),
      16#2b#        => ("movntss         ", C_Mss, C_Vss, Extra_None),
      16#2c#        => ("cvttss2si       ", C_Gy, C_Wss, Extra_None),
      16#2d#        => ("cvtss2si        ", C_Gy, C_Wss, Extra_None),

      --  The 16#50# slot is reserved
      16#51#        => ("sqrtss          ", C_Vss, C_Wss, Extra_None),
      16#52#        => ("rsqrtss         ", C_Vss, C_Wss, Extra_None),
      16#53#        => ("rcpss           ", C_Vss, C_Wss, Extra_None),
      --  The 16#54#-16#57# slots are reserved
      16#58#        => ("addss           ", C_Vss, C_Wss, Extra_None),
      16#59#        => ("mulss           ", C_Vss, C_Wss, Extra_None),
      16#5a#        => ("cvtss2sd        ", C_Vsd, C_Wss, Extra_None),
      16#5b#        => ("cvttps2dq       ", C_Vdq, C_Wps, Extra_None),
      16#5c#        => ("subss           ", C_Vss, C_Wss, Extra_None),
      16#5d#        => ("minss           ", C_Vss, C_Wss, Extra_None),
      16#5e#        => ("divss           ", C_Vss, C_Wss, Extra_None),
      16#5f#        => ("maxss           ", C_Vss, C_Wss, Extra_None),

      16#6f#        => ("movdqu          ", C_Vdq, C_Wdq, Extra_None),

      16#70#        => ("pshufhw         ", C_Vdq, C_Wdq, Extra_8),
      --  TODO??? 12/13/14 extended opcodes forms
      --  The 16#74#-16#7d# slots are reserved
      16#7e#        => ("movq            ", C_Vq, C_Wq, Extra_None),
      16#7f#        => ("movdqu          ", C_Wdq, C_Vdq, Extra_None),

      16#b8#        => ("popcnt          ", C_Gv, C_Ev, Extra_None),
      --  TODO??? 8 extended opcodes forms
      16#bc#        => ("tzcnt           ", C_Gv, C_Ev, Extra_None),
      16#bd#        => ("lzcnt           ", C_Gv, C_Ev, Extra_None),

      16#c2#        => ("cmpss           ", C_Vss, C_Wss, Extra_8),
      16#d6#        => ("movq2dq         ", C_Vdq, C_Qq, Extra_None),
      16#e6#        => ("cvtdq2pd        ", C_Vpd, C_Wq, Extra_None),

      others        => ("                ", C_None, C_None, Extra_None));

   --  Mnemonics for the "group 1" and "group 2" of instructions. For these,
   --  the mnemonic is determined by the "reg/opcode" 3-bit field of the ModR/M
   --  byte, and the operand kinds are determined by the first byte of the
   --  opcode sequence. (see the 16#80#-16#83# and  entries of the
   --  corresponding lookup table).

   subtype String3 is String (1 .. 3);
   type Group_Name_Array_Type is array (Bit_Field_3) of String3;

   --  The group 1 is used for the first opcode bytes from 16#80# to 16#83#

   Group_Name_1 : constant Group_Name_Array_Type :=
     ("add", "or ", "adc", "sbb", "and", "sub", "xor", "cmp");

   --  The group 2 is used for the first opcode bytes 16#c0#, 16#c1#, 16#d0#,
   --  16#d1#, 16#d2# and 16#d3#.

   Group_Name_2 : constant Group_Name_Array_Type :=
     ("rol", "ror", "rcl", "rcr", "shl", "shr", "   ", "sar");

   --  Lookup tables for some "group"s of instructions

   --  The group 3 is used for the first opcode bytes 16#f6# and 16#f7#

   Insn_Desc_G3 : constant Group_Desc_Array_Type :=
     (2#000# => ("test            ", C_Ib, C_Iz, Extra_None),
      2#010# => ("not             ", C_None, C_None, Extra_None),
      2#011# => ("neg             ", C_None, C_None, Extra_None),
      2#100# => ("mul             ", C_Reg_Al, C_Reg_Ax, Extra_None),
      2#101# => ("imul            ", C_Reg_Al, C_Reg_Ax, Extra_None),
      2#110# => ("div             ", C_Reg_Al, C_Reg_Ax, Extra_None),
      2#111# => ("idiv            ", C_Reg_Al, C_Reg_Ax, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  The group 4 is used for the first opcode byte 16#fe#

   Insn_Desc_G4 : constant Group_Desc_Array_Type :=
     (2#000# => ("inc             ", C_Eb, C_None, Extra_None),
      2#001# => ("dec             ", C_Eb, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  The group 5 is used for the first opcode byte 16#ff#

   Insn_Desc_G5 : constant Group_Desc_Array_Type :=
     (2#000# => ("inc             ", C_Ev, C_None, Extra_None),
      2#001# => ("dec             ", C_Ev, C_None, Extra_None),
      2#010# => ("call            ", C_Ev, C_None, Extra_None),
      2#011# => ("callf           ", C_Ep, C_None, Extra_None),
      2#100# => ("jmp             ", C_Ev, C_None, Extra_None),
      2#101# => ("jmpf            ", C_Ep, C_None, Extra_None),
      2#110# => ("push            ", C_Ev, C_None, Extra_None),
      2#111# => ("                ", C_None, C_None, Extra_None));

   --  The group 6 is used for the second opcode byte 16#00#

   Insn_Desc_G6 : constant Group_Desc_Array_Type :=
     (2#000# => ("sldt            ", C_Ew, C_None, Extra_None),
      2#001# => ("str             ", C_Ew, C_None, Extra_None),
      2#010# => ("lldt            ", C_Ew, C_None, Extra_None),
      2#011# => ("ltr             ", C_Ew, C_None, Extra_None),
      2#100# => ("verr            ", C_Ew, C_None, Extra_None),
      2#101# => ("verw            ", C_Ew, C_None, Extra_None),
      2#110# => ("                ", C_None, C_None, Extra_None),
      2#111# => ("                ", C_None, C_None, Extra_None));

   --  The group 7 is used for the second opcode byte 16#01#

   Insn_Desc_G7 : constant Group_Desc_Array_Type :=
     (2#000# => ("sgdt            ", C_Ms, C_None, Extra_None),
      2#001# => ("sidt            ", C_Ms, C_None, Extra_None),
      2#010# => ("lgdt            ", C_Ms, C_None, Extra_None),
      2#011# => ("lidt            ", C_Ms, C_None, Extra_None),
      2#100# => ("smsw            ", C_Ew, C_None, Extra_None),
      2#101# => ("                ", C_None, C_None, Extra_None),
      2#110# => ("lmsw            ", C_Ew, C_None, Extra_None),
      2#111# => ("invlpg          ", C_Mb, C_None, Extra_None));

   Insn_Desc_G7_000_mod11 : constant Sub_Group_Desc_Array_Type :=
     (2#001# => ("vmcall          ", C_None, C_None, Extra_None),
      2#010# => ("vmlaunch        ", C_None, C_None, Extra_None),
      2#011# => ("vmresume        ", C_None, C_None, Extra_None),
      2#100# => ("vmxoff          ", C_None, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G7_001_mod11 : constant Sub_Group_Desc_Array_Type :=
     (2#000# => ("monitor         ", C_None, C_None, Extra_None),
      2#001# => ("mwait           ", C_None, C_None, Extra_None),
      2#010# => ("clac            ", C_None, C_None, Extra_None),
      2#011# => ("stac            ", C_None, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G7_010_mod11 : constant Sub_Group_Desc_Array_Type :=
     (2#000# => ("xgetbv          ", C_None, C_None, Extra_None),
      2#001# => ("xsetbv          ", C_None, C_None, Extra_None),
      2#100# => ("vmfunc          ", C_None, C_None, Extra_None),
      2#101# => ("xend            ", C_None, C_None, Extra_None),
      2#110# => ("xtest           ", C_None, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  The group 8 is used for the second opcode byte 16#ba#

   Insn_Desc_G8 : constant Group_Desc_Array_Type :=
     (2#100# => ("bt              ", C_Ev, C_Ib, Extra_None),
      2#101# => ("bts             ", C_Ev, C_Ib, Extra_None),
      2#110# => ("btr             ", C_Ev, C_Ib, Extra_None),
      2#111# => ("btc             ", C_Ev, C_Ib, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  The group 9 is used for the second opcode byte 16#c7#
   --
   Insn_Desc_G9_mem : constant Group_Desc_Array_Type :=
     (2#001# => ("cmpxch8b        ", C_Mq, C_None, Extra_None),
      2#110# => ("vmptrld         ", C_Mq, C_None, Extra_None),
      2#111# => ("vmptrst         ", C_Mq, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G9_mem_66 : constant Group_Desc_Array_Type :=
     (2#110# => ("vmclear         ", C_Mq, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G9_mem_F3 : constant Group_Desc_Array_Type :=
     (2#110# => ("vmxon           ", C_Mq, C_None, Extra_None),
      2#111# => ("vmptrst         ", C_Mq, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G9_mod11 : constant Group_Desc_Array_Type :=
     (2#110# => ("rdrand          ", C_Rv, C_None, Extra_None),
      2#111# => ("rdseed          ", C_Rv, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  Groups 12-14 are used only with a "mod" ModR/M byte field equal to "11"

   type Groups_12_13_14_Desc_Array_Type is
      array (Character range '2' .. '4', Bit_Field_3)
      of Insn_Desc_Type;

   Insn_Desc_G12_13_14_mod11 : constant Groups_12_13_14_Desc_Array_Type :=
     ('2' =>
        (2#010# => ("psrlw           ", C_Nq, C_Ib, Extra_None),
         2#100# => ("psraw           ", C_Nq, C_Ib, Extra_None),
         2#110# => ("psllw           ", C_Nq, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)),
      '3' =>
        (2#010# => ("psrld           ", C_Nq, C_Ib, Extra_None),
         2#100# => ("psrad           ", C_Nq, C_Ib, Extra_None),
         2#110# => ("pslld           ", C_Nq, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)),
      '4' =>
        (2#010# => ("psrlq           ", C_Nq, C_Ib, Extra_None),
         2#110# => ("psllq           ", C_Nq, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)));

   Insn_Desc_G12_13_14_mod11_66 : constant Groups_12_13_14_Desc_Array_Type :=
     ('2' =>
        (2#010# => ("psrlw           ", C_Ux, C_Ib, Extra_None),
         2#100# => ("psraw           ", C_Ux, C_Ib, Extra_None),
         2#110# => ("psllw           ", C_Ux, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)),
      '3' =>
        (2#010# => ("psrld           ", C_Ux, C_Ib, Extra_None),
         2#100# => ("psrad           ", C_Ux, C_Ib, Extra_None),
         2#110# => ("pslld           ", C_Ux, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)),
      '4' =>
        (2#010# => ("psrlq           ", C_Ux, C_Ib, Extra_None),
         2#011# => ("psrldq          ", C_Ux, C_Ib, Extra_None),
         2#110# => ("psllq           ", C_Ux, C_Ib, Extra_None),
         2#111# => ("pslldq          ", C_Ux, C_Ib, Extra_None),
         others => ("                ", C_None, C_None, Extra_None)));

   Insn_Desc_G15 : constant Group_Desc_Array_Type :=
     (2#000# => ("fxsave          ", C_M, C_None, Extra_None),
      2#001# => ("fxrstor         ", C_M, C_None, Extra_None),
      2#010# => ("ldmxcsr         ", C_Md, C_None, Extra_None),
      2#011# => ("stmxcsr         ", C_Md, C_None, Extra_None),
      2#100# => ("xsave           ", C_M, C_None, Extra_None),
      2#101# => ("xrstor          ", C_M, C_None, Extra_None),
      2#110# => ("xsaveopt        ", C_M, C_None, Extra_None),
      2#111# => ("clflush         ", C_Mb, C_None, Extra_None));

   Insn_Desc_G15_mod11 : constant Group_Desc_Array_Type :=
     (2#101# => ("lfence          ", C_None, C_None, Extra_None),
      2#110# => ("mfence          ", C_None, C_None, Extra_None),
      2#111# => ("sfence          ", C_None, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   Insn_Desc_G16 : constant Group_Desc_Array_Type :=
     (2#000# => ("prefetchnta     ", C_M, C_None, Extra_None),
      2#001# => ("prefetcht0      ", C_M, C_None, Extra_None),
      2#010# => ("prefetcht1      ", C_M, C_None, Extra_None),
      2#011# => ("prefetcht2      ", C_M, C_None, Extra_None),
      others => ("                ", C_None, C_None, Extra_None));

   --  Two-levels lookup table for escape to coprocessor instruction set, when
   --  the ModR/M byte is inside the range from 16#00# to 16#bf# (included). It
   --  is indexed first by the 3 least significant bits of the first opcode
   --  byte, then by the "reg/opcode" 3-bit field of the ModR/M byte.

   type Esc_Inside_Desc_Array_Type is
      array (Bit_Field_3, Bit_Field_3) of Insn_Desc_Type;

   Insn_Desc_Esc_Before_Bf : constant Esc_Inside_Desc_Array_Type :=
     (
      --  D8
      (2#000# => ("fadd            ", C_Mfs, C_None, Extra_None),
       2#001# => ("fmul            ", C_Mfs, C_None, Extra_None),
       2#010# => ("fcom            ", C_Mfs, C_None, Extra_None),
       2#011# => ("fcomp           ", C_Mfs, C_None, Extra_None),
       2#100# => ("fsub            ", C_Mfs, C_None, Extra_None),
       2#101# => ("fsubr           ", C_Mfs, C_None, Extra_None),
       2#110# => ("fdiv            ", C_Mfs, C_None, Extra_None),
       2#111# => ("fdivr           ", C_Mfs, C_None, Extra_None)),
      --  D9
      (2#000# => ("fld             ", C_Mfs, C_None, Extra_None),
       2#001# => ("                ", C_None, C_None, Extra_None),
       2#010# => ("fst             ", C_Mfs, C_None, Extra_None),
       2#011# => ("fstp            ", C_Mfs, C_None, Extra_None),
       2#100# => ("fldenv          ", C_M, C_None, Extra_None),
       2#101# => ("fldcw           ", C_Mfs, C_None, Extra_None),
       2#110# => ("fstenv          ", C_Mfs, C_None, Extra_None),
       2#111# => ("fstcw           ", C_Mfs, C_None, Extra_None)),
      --  DA
      (2#000# => ("fiadd           ", C_Md, C_None, Extra_None),
       2#001# => ("fimul           ", C_Md, C_None, Extra_None),
       2#010# => ("ficom           ", C_Md, C_None, Extra_None),
       2#011# => ("ficomp          ", C_Md, C_None, Extra_None),
       2#100# => ("fisub           ", C_Md, C_None, Extra_None),
       2#101# => ("fisubr          ", C_Md, C_None, Extra_None),
       2#110# => ("fidiv           ", C_Md, C_None, Extra_None),
       2#111# => ("fidivr          ", C_Md, C_None, Extra_None)),
      --  DB
      (2#000# => ("fild            ", C_Md, C_None, Extra_None),
       2#001# => ("fisttp          ", C_Md, C_None, Extra_None),
       2#010# => ("fist            ", C_Md, C_None, Extra_None),
       2#011# => ("fistp           ", C_Md, C_None, Extra_None),
       2#100# => ("                ", C_None, C_None, Extra_None),
       2#101# => ("fld             ", C_Mfe, C_None, Extra_None),
       2#110# => ("                ", C_None, C_None, Extra_None),
       2#111# => ("fstp            ", C_Mfe, C_None, Extra_None)),
      --  DC
      (2#000# => ("fadd            ", C_Mfd, C_None, Extra_None),
       2#001# => ("fmul            ", C_Mfd, C_None, Extra_None),
       2#010# => ("fcom            ", C_Mfd, C_None, Extra_None),
       2#011# => ("fcomp           ", C_Mfd, C_None, Extra_None),
       2#100# => ("fsub            ", C_Mfd, C_None, Extra_None),
       2#101# => ("fsubr           ", C_Mfd, C_None, Extra_None),
       2#110# => ("fdiv            ", C_Mfd, C_None, Extra_None),
       2#111# => ("fdivr           ", C_Mfd, C_None, Extra_None)),
      --  DD
      (2#000# => ("fld             ", C_Mfd, C_None, Extra_None),
       2#001# => ("fisttp          ", C_Mq, C_None, Extra_None),
       2#010# => ("fst             ", C_Mfd, C_None, Extra_None),
       2#011# => ("fstp            ", C_Mfd, C_None, Extra_None),
       2#100# => ("frstor          ", C_M, C_None, Extra_None),
       2#101# => ("                ", C_None, C_None, Extra_None),
       2#110# => ("fsave           ", C_M, C_None, Extra_None),
       2#111# => ("fstsw           ", C_M, C_None, Extra_None)),
      --  DE
      (2#000# => ("fiadd           ", C_Mw, C_None, Extra_None),
       2#001# => ("fimul           ", C_Mw, C_None, Extra_None),
       2#010# => ("ficom           ", C_Mw, C_None, Extra_None),
       2#011# => ("ficomp          ", C_Mw, C_None, Extra_None),
       2#100# => ("fisub           ", C_Mw, C_None, Extra_None),
       2#101# => ("fisubr          ", C_Mw, C_None, Extra_None),
       2#110# => ("fidiv           ", C_Mw, C_None, Extra_None),
       2#111# => ("fidivr          ", C_Mw, C_None, Extra_None)),
      --  DF
      (2#000# => ("fild            ", C_Md, C_None, Extra_None),
       2#001# => ("fisttp          ", C_Md, C_None, Extra_None),
       2#010# => ("fist            ", C_Md, C_None, Extra_None),
       2#011# => ("fistp           ", C_Md, C_None, Extra_None),
       2#100# => ("fbld            ", C_M, C_None, Extra_None),
       2#101# => ("fild            ", C_Mq, C_None, Extra_None),
       2#110# => ("fbstp           ", C_M, C_None, Extra_None),
       2#111# => ("fistp           ", C_Mq, C_None, Extra_None)));

   --  Two-levels lookup table for escape to coprocessor instruction set, when
   --  the ModR/M byte is outside the range from 16#00# to 16#bf# (included).
   --  It is indexed first by the 3 least significant bits of the first opcode
   --  byte, then by the ModR/M byte.

   type Esc_Outside_Modrm_Type is range 16#c0# .. 16#ff#;
   type Esc_Outside_Desc_Array_Type is
      array (Bit_Field_3, Esc_Outside_Modrm_Type) of Insn_Desc_Type;

   Insn_Desc_Esc_After_Bf : constant Esc_Outside_Desc_Array_Type :=
     (
      --  D8
      (16#c0# .. 16#c7# => ("fadd            ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fmul            ", C_H0, C_H, Extra_None),
       16#d0# .. 16#d7# => ("fcom            ", C_H0, C_H, Extra_None),
       16#d8# .. 16#df# => ("fcomp           ", C_H0, C_H, Extra_None),
       16#e0# .. 16#e7# => ("fsub            ", C_H0, C_H, Extra_None),
       16#e8# .. 16#ef# => ("fsubr           ", C_H0, C_H, Extra_None),
       16#f0# .. 16#f7# => ("fdiv            ", C_H0, C_H, Extra_None),
       16#f8# .. 16#ff# => ("fdivr           ", C_H0, C_H, Extra_None)),
      --  D9
      (16#c0# .. 16#c7# => ("fld             ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fxch            ", C_H0, C_H, Extra_None),
       16#d0#           => ("fnop            ", C_None, C_None, Extra_None),

       16#e0#           => ("fchs            ", C_None, C_None, Extra_None),
       16#e1#           => ("fabs            ", C_None, C_None, Extra_None),
       16#e4#           => ("ftst            ", C_None, C_None, Extra_None),
       16#e5#           => ("fxam            ", C_None, C_None, Extra_None),
       16#e8#           => ("fld1            ", C_None, C_None, Extra_None),
       16#e9#           => ("fldl2t          ", C_None, C_None, Extra_None),
       16#ea#           => ("fldl2e          ", C_None, C_None, Extra_None),
       16#eb#           => ("fldpi           ", C_None, C_None, Extra_None),
       16#ec#           => ("fldlg2          ", C_None, C_None, Extra_None),
       16#ed#           => ("fldln2          ", C_None, C_None, Extra_None),
       16#ee#           => ("fldlz           ", C_None, C_None, Extra_None),

       16#f0#           => ("f2xm1           ", C_None, C_None, Extra_None),
       16#f1#           => ("fyl2x           ", C_None, C_None, Extra_None),
       16#f2#           => ("fptan           ", C_None, C_None, Extra_None),
       16#f3#           => ("fpatan          ", C_None, C_None, Extra_None),
       16#f4#           => ("fpxtract        ", C_None, C_None, Extra_None),
       16#f5#           => ("fprem1          ", C_None, C_None, Extra_None),
       16#f6#           => ("fdecstp         ", C_None, C_None, Extra_None),
       16#f7#           => ("fincstp         ", C_None, C_None, Extra_None),
       16#f8#           => ("fprem           ", C_None, C_None, Extra_None),
       16#f9#           => ("fyl2xp1         ", C_None, C_None, Extra_None),
       16#fa#           => ("fsqrt           ", C_None, C_None, Extra_None),
       16#fb#           => ("fsincos         ", C_None, C_None, Extra_None),
       16#fc#           => ("frndint         ", C_None, C_None, Extra_None),
       16#fd#           => ("fscale          ", C_None, C_None, Extra_None),
       16#fe#           => ("fsin            ", C_None, C_None, Extra_None),
       16#ff#           => ("fcos            ", C_None, C_None, Extra_None),

       others           => ("                ", C_None, C_None, Extra_None)),
      --  DA
      (16#c0# .. 16#c7# => ("fcmovb          ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fcmove          ", C_H0, C_H, Extra_None),
       16#d0# .. 16#d7# => ("fcmovbe         ", C_H0, C_H, Extra_None),
       16#d8# .. 16#df# => ("fcmovu          ", C_H0, C_H, Extra_None),
       16#e9#           => ("fucompp         ", C_None, C_None, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)),
      --  DB
      (16#c0# .. 16#c7# => ("fcmovnb         ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fcmovne         ", C_H0, C_H, Extra_None),
       16#d0# .. 16#d7# => ("fcmovnbe        ", C_H0, C_H, Extra_None),
       16#d8# .. 16#df# => ("fcmovnu         ", C_H0, C_H, Extra_None),
       16#e8# .. 16#ef# => ("fucomi          ", C_H0, C_H, Extra_None),
       16#f0# .. 16#f7# => ("fcomi           ", C_H0, C_H, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)),
      --  DC
      (16#c0# .. 16#c7# => ("fadd            ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fmul            ", C_H0, C_H, Extra_None),
       16#e0# .. 16#e7# => ("fsubr           ", C_H0, C_H, Extra_None),
       16#e8# .. 16#ef# => ("fsub            ", C_H0, C_H, Extra_None),
       16#f0# .. 16#f7# => ("fdivr           ", C_H0, C_H, Extra_None),
       16#f8# .. 16#ff# => ("fdiv            ", C_H0, C_H, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)),
      --  DD
      (16#c0# .. 16#c7# => ("ffree           ", C_H, C_None, Extra_None),
       16#d0# .. 16#d7# => ("fst             ", C_H, C_None, Extra_None),
       16#d8# .. 16#df# => ("fstp            ", C_H, C_None, Extra_None),
       16#e0# .. 16#e7# => ("fucom           ", C_H, C_H0, Extra_None),
       16#e8# .. 16#ef# => ("fucomp          ", C_H, C_None, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)),
      --  DE
      (16#c0# .. 16#c7# => ("faddp           ", C_H0, C_H, Extra_None),
       16#c8# .. 16#cf# => ("fmulp           ", C_H0, C_H, Extra_None),
       16#d9#           => ("fcompp          ", C_None, C_None, Extra_None),
       16#e0# .. 16#e7# => ("fsubrp          ", C_H0, C_H, Extra_None),
       16#e8# .. 16#ef# => ("fsubp           ", C_H0, C_H, Extra_None),
       16#f0# .. 16#f7# => ("fdivrp          ", C_H0, C_H, Extra_None),
       16#f8# .. 16#ff# => ("fdivp           ", C_H0, C_H, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)),
      --  DF
      (16#e0#           => ("fstsw           ", C_Reg_Ax, C_None, Extra_None),
       16#e8# .. 16#ef# => ("fucomip         ", C_H0, C_H, Extra_None),
       16#f0# .. 16#f7# => ("fcompip         ", C_H0, C_H, Extra_None),
       others           => ("                ", C_None, C_None, Extra_None)));

   --  Standard widths of operations

   type Width_Array_Type is array (Width_Type) of Character;
   Width_Char : constant Width_Array_Type :=
     (W_None => '-',
      W_8    => 'b',
      W_16   => 'w',
      W_32   => 'l',
      W_64   => 'q',
      W_128  => 's');
   --  Turn an operand size into a character, for debugging purposes

   type Width_Len_Type is array (Width_Type) of Pc_Type;
   Width_Len : constant Width_Len_Type :=
     (W_None => 0,
      W_8    => 1,
      W_16   => 2,
      W_32   => 4,
      W_64   => 8,
      W_128  => 16);
   --  Turn an operand size into the corresponding number of bytes

   type To_General_Type is array (Width_Type) of Reg_Class_Type;
   To_General : constant To_General_Type :=
     (W_None => R_None,
      W_8    => R_8,
      W_16   => R_16,
      W_32   => R_32,
      W_64   => R_64,
      W_128  => R_None);
   --  Turn an operand size into the corresponding general-purpose register
   --  class (if any, R_None otherwise).

   type To_Z_Type is array (Width_Type) of Width_Type;
   To_Z : constant To_Z_Type :=
     (W_None => W_None,
      W_8    => W_None,
      W_16   => W_16,
      W_32   => W_32,
      W_64   => W_32,
      W_128  => W_None);
   --  Turn an operand size to W_None if it is not a valid value for the
   --  operand-size attribut.

   type To_Register_Number_Type is array (GPR_Code) of Bit_Field_3;
   To_Register_Number : constant To_Register_Number_Type :=
     (C_Reg_Al => 2#000#,
      C_Reg_Cl => 2#001#,
      C_Reg_Dl => 2#010#,
      C_Reg_Bl => 2#011#,
      C_Reg_Ah => 2#100#,
      C_Reg_Ch => 2#101#,
      C_Reg_Dh => 2#110#,
      C_Reg_Bh => 2#111#,
      C_Reg_Ax => 2#000#,
      C_Reg_Cx => 2#001#,
      C_Reg_Dx => 2#010#,
      C_Reg_Bx => 2#011#,
      C_Reg_Sp => 2#100#,
      C_Reg_Bp => 2#101#,
      C_Reg_Si => 2#110#,
      C_Reg_Di => 2#111#);
   --  Turn a specific register operand kind into a 3-bit field suitable for
   --  the Add_Reg procedure.

   type To_Register_Segment_Type is array (Prefix_Seg_Code) of Bit_Field_3;
   To_Register_Segment : constant To_Register_Segment_Type :=
     (C_Prefix_Cs => 2#001#,
      C_Prefix_Ss => 2#010#,
      C_Prefix_Ds => 2#011#,
      C_Prefix_Es => 2#000#,
      C_Prefix_Fs => 2#100#,
      C_Prefix_Gs => 2#101#);
   --  Turn a segment prefix operand kind into a 3-bit field suitable for the
   --  Add_Reg_Seg procedure.

   --  Bits extraction from byte functions

   --  For a byte, MSB (most significant bit) is bit 7 while LSB (least
   --  significant bit) is bit 0.

   function Ext_0 (B : Byte) return Bit;
   function Ext_1 (B : Byte) return Bit;
   function Ext_2 (B : Byte) return Bit;
   function Ext_3 (B : Byte) return Bit;

   function Ext_210 (B : Byte) return Bit_Field_3;
   pragma Inline (Ext_210);
   --  Extract bits 2, 1 and 0

   function Ext_543 (B : Byte) return Bit_Field_3;
   pragma Inline (Ext_543);
   --  Extract bits 5-3 of byte B

   function Ext_76 (B : Byte) return Bit_Field_2;
   pragma Inline (Ext_76);
   --  Extract bits 7-6 of byte B

   Bad_Memory : exception;

   type Mem_Read is access function (Off : Pc_Type) return Byte;

   function Decode_Val
     (Mem            : Mem_Read;
      Off            : Pc_Type;
      Width          : Width_Type;
      Sign_Extend    : Boolean)
     return Unsigned_64;
   --  Decode an immediate value given its memory location, its size and its
   --  signedness.
   --  Off is the immediate address and is relative to a certain PC. Mem is a
   --  function that reads one byte at an offset from this PC.

   function Truncate_To_Pc_Type (Value : Unsigned_64) return Pc_Type is
     (Pc_Type (Value and Unsigned_64 (Pc_Type'Last)));

   -----------
   -- Ext_0 --
   -----------

   function Ext_0 (B : Byte) return Bit is
   begin
      return Bit (B and 1);
   end Ext_0;

   -----------
   -- Ext_1 --
   -----------

   function Ext_1 (B : Byte) return Bit is
   begin
      return Bit (Shift_Right (B, 1) and 1);
   end Ext_1;

   -----------
   -- Ext_2 --
   -----------

   function Ext_2 (B : Byte) return Bit is
   begin
      return Bit (Shift_Right (B, 2) and 1);
   end Ext_2;

   -----------
   -- Ext_3 --
   -----------

   function Ext_3 (B : Byte) return Bit is
   begin
      return Bit (Shift_Right (B, 3) and 1);
   end Ext_3;

   -------------
   -- Ext_210 --
   -------------

   function Ext_210 (B : Byte) return Bit_Field_3 is
   begin
      return Bit_Field_3 (B and 2#111#);
   end Ext_210;

   -------------
   -- Ext_543 --
   -------------

   function Ext_543 (B : Byte) return Bit_Field_3 is
   begin
      return Bit_Field_3 (Shift_Right (B, 3) and 2#111#);
   end Ext_543;

   ------------
   -- Ext_76 --
   ------------

   function Ext_76 (B : Byte) return Bit_Field_2 is
   begin
      return Bit_Field_2 (Shift_Right (B, 6) and 2#11#);
   end Ext_76;

   function Ext_Modrm_Mod (B : Byte) return Bit_Field_2 renames Ext_76;
   function Ext_Modrm_Rm  (B : Byte) return Bit_Field_3 renames Ext_210;
   function Ext_Modrm_Reg (B : Byte) return Bit_Field_3 renames Ext_543;

   function Ext_Sib_Base  (B : Byte) return Bit_Field_3 renames Ext_210;
   function Ext_Sib_Index (B : Byte) return Bit_Field_3 renames Ext_543;
   function Ext_Sib_Scale (B : Byte) return Bit_Field_2 renames Ext_76;

   function Ext_Rex_B     (B : Byte) return Bit renames Ext_0;
   function Ext_Rex_X     (B : Byte) return Bit renames Ext_1;
   function Ext_Rex_R     (B : Byte) return Bit renames Ext_2;
   function Ext_Rex_W     (B : Byte) return Bit renames Ext_3;

   type Hex_Str is array (Natural range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Str := "0123456789abcdef";

   ----------------
   -- Decode_Val --
   ----------------

   function Decode_Val
     (Mem         : Mem_Read;
      Off         : Pc_Type;
      Width       : Width_Type;
      Sign_Extend : Boolean)
     return Unsigned_64
   is
      Is_Negative : Boolean;
      V : Unsigned_64;

      subtype Sign_Extension_Width_Type is Width_Type range W_8 .. W_64;
      type Sign_Extension_Type is
         array (Sign_Extension_Width_Type) of Unsigned_64;
      Sign_Extension : constant Sign_Extension_Type :=
        (16#ffff_ffff_ffff_ff00#,
         16#ffff_ffff_ffff_0000#,
         16#ffff_ffff_0000_0000#,
         16#0000_0000_0000_0000#);
   begin
      --  For each size, once the value is read from memory, sign extend if
      --  needed.

      case Width is
         when W_8 =>
            V := Unsigned_64 (Mem (Off));
            Is_Negative := Sign_Extend and then V >= 16#80#;

         when W_16 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Unsigned_64 (Mem (Off));
            Is_Negative := Sign_Extend and then V >= 16#8000#;

         when W_32 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 3)), 24)
              or Shift_Left (Unsigned_64 (Mem (Off + 2)), 16)
              or Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Shift_Left (Unsigned_64 (Mem (Off + 0)), 0);
            Is_Negative := Sign_Extend and then V >= 16#8000_0000#;

         when W_64 =>
            V := Shift_Left (Unsigned_64 (Mem (Off + 7)), 56)
              or Shift_Left (Unsigned_64 (Mem (Off + 6)), 48)
              or Shift_Left (Unsigned_64 (Mem (Off + 5)), 40)
              or Shift_Left (Unsigned_64 (Mem (Off + 4)), 32)
              or Shift_Left (Unsigned_64 (Mem (Off + 3)), 24)
              or Shift_Left (Unsigned_64 (Mem (Off + 2)), 16)
              or Shift_Left (Unsigned_64 (Mem (Off + 1)), 8)
              or Shift_Left (Unsigned_64 (Mem (Off + 0)), 0);
            Is_Negative := Sign_Extend and then V >= 16#8000_0000_0000_0000#;

         when W_128 =>
            raise Invalid_Insn with "invalid 128-bit immediate decoding";

         when W_None =>
            raise Invalid_Insn;
      end case;

      if Is_Negative then
         V := Sign_Extension (Width) or V;
      end if;

      return V;
   end Decode_Val;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Buffer   : in out Highlighting.Buffer_Type;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Pc);

      Is_64bit   : constant Boolean := Machine = Elf_Common.EM_X86_64;
      Rex_Prefix : Byte             := 0;

      function Mem (Off : Pc_Type) return Byte;
      --  The instruction memory, 0 based

      function Reg_With_Rex (Rex_Prefix : Bit; Reg : Bit_Field_3)
         return Bit_Field_4;
      --  Combine the given REX prefix bit and the given 3-bit register number
      --  into an "extended" 4-bit register number.

      ----------------------------
      -- Basic output utilities --
      ----------------------------

      --  The following functions add basic elements (a character, an
      --  instruction mnemonic, a byte, etc.) to the output line.

      procedure Add_Name (Name : String);
      pragma Inline (Add_Name);

      procedure Add_Hex
        (Value  : Unsigned_64;
         Signed : Boolean;
         Width  : Width_Type);
      --  Add Value as an hexadecimal token to Buffer. Interpret it as a 64-bit
      --  signed value if Signed. If Width is W_None, output as few digits as
      --  needed, otherwise, output a "fixed-length" literal corresponding to
      --  the given length.

      procedure Add_Comma;

      procedure Name_Align (Orig : Natural);
      --  Pad the current line after the mnemonic with blanks to align operands
      --  up to 8 columns after Orig, which is the mnemonic starting column.

      procedure Add_Reg (F : Bit_Field_4; R : Reg_Class_Type);
      --  Add a register name to the output, given its 3-bit field
      --  identificator and its register class.

      procedure Add_Reg_St (F : Bit_Field_3);
      --  Add a coprocessor stack register name to the output

      procedure Add_Reg_Seg (F : Bit_Field_3);
      --  Add a segment register name to the output

      ------------------------------------
      -- Instruction decoding utilities --
      ------------------------------------

      --  The following functions decode various instruction fields *and*
      --  directly add them to the output line.

      procedure Decode_Val
        (Off    : Pc_Type;
         Width  : Width_Type;
         Signed : Boolean);
      --  Add the value in the binary at the given "Off" offset of the given
      --  "Width" to the output.

      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type);
      --  Add the value in the binary at the given "Off" offset of the given
      --  "Width" to the output as an assembly immediate (with the "$0x"
      --  prefix) and update "Off" to point to the first byte past the
      --  immediate.

      procedure Decode_Disp (Off           : Pc_Type;
                             Width         : Width_Type;
                             Lookup_Symbol : Boolean;
                             Offset        : Unsigned_32 := 0);
      --  Add the displacement value of the given "Width" in the binary at the
      --  address "Off" plus the given "Offset" to the output.

      procedure Decode_Disp_Rel (Off : in out Pc_Type;
                                 Width : Width_Type);
      --  Add the relative displacement in the binary at the address "Off" to
      --  the output and update "Off" to point to the first byte past the
      --  immediate.

      procedure Decode_Sib (Sib : Byte; B_Mod : Bit_Field_2);
      --  Decode the displacement encoded in the given SIB byte given the
      --  previous "mod" ModR/M field and add it to the output.

      procedure Decode_Modrm_Reg (B : Byte; R : Reg_Class_Type);
      --  Decode the given ModR/M byte "reg/opcode" field according to the
      --  given register class and add the decoded operand to the output.

      procedure Decode_Modrm_Mem (Off : Pc_Type; R : Reg_Class_Type);
      --  Decode the ModR/M byte at address "Off" in the binary according to
      --  the given register class and add the decoded operand to the output.

      function Decode_Modrm_Len (Off : Pc_Type) return Pc_Type;
      --  Return the byte length of the ModR/M+SIB+displacement bytes, if
      --  present, given the address "Off" of the ModR/M byte.

      procedure Add_Operand (C : Code_Type;
                             Off_Modrm : Pc_Type;
                             Off_Imm : in out Pc_Type;
                             W : Width_Type);
      --  Decode and add to the output the operand corresponding to the given
      --  operand kind "C", given the address of the ModR/M byte, the
      --  operand-size attribute and the address of the immediate value. Update
      --  the address of the immediate value to point to the first byte after
      --  it, if any.

      procedure Update_Length (C : Code_Type;
                               Off_Imm : in out Pc_Type;
                               W : Width_Type);
      --  Increment the given "Off_Imm" offset using the size of the operand
      --  given its kind "C" and the operand-size attribute.

      procedure Add_Opcode (Name : String16; Width : Width_Type);
      pragma Unreferenced (Add_Opcode);
      --  XXX

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name (Name : String) is
      begin
         for I in Name'Range loop
            exit when Name (I) = ' ';
            Buffer.Put (Name (I));
         end loop;
      end Add_Name;

      ---------------
      -- Add_Comma --
      ---------------

      procedure Add_Comma is
      begin
         Buffer.Start_Token (Punctuation);
         Buffer.Put (',');
         Buffer.Start_Token (Text);
         Buffer.Put (' ');
      end Add_Comma;

      ----------------
      -- Name_Align --
      ----------------

      procedure Name_Align (Orig : Natural) is
      begin
         Buffer.Start_Token (Text);
         Buffer.Put (' ');
         Buffer.Put ((Buffer.Last_Index + 1
                     .. (Orig + 8 - 1) => ' '));
      end Name_Align;

      ----------------
      -- Add_Opcode --
      ----------------

      procedure Add_Opcode (Name : String16; Width : Width_Type) is
         L : constant Natural := Buffer.Last_Index;
      begin
         Buffer.Put (Name);

         --  Debugging utility: add operand data size suffix to the mnemonic

         if Debug and then Width /= W_None then
            Buffer.Put (Width_Char (Width));
         end if;

         Name_Align (L);
      end Add_Opcode;

      ----------------
      -- Add_Reg_St --
      ----------------

      procedure Add_Reg_St (F : Bit_Field_3) is
      begin
         Buffer.Start_Token (Register);
         Buffer.Put ("%st");
         Buffer.Start_Token (Punctuation);
         Buffer.Put ('(');
         Buffer.Start_Token (Literal);
         Buffer.Put (Hex_Digit (Natural (F)));
         Buffer.Start_Token (Punctuation);
         Buffer.Put (')');
      end Add_Reg_St;

      -----------------
      -- Add_Reg_Seg --
      -----------------

      procedure Add_Reg_Seg (F : Bit_Field_3) is
      begin
         Buffer.Start_Token (Register);
         case F is
            when 2#000# =>
               Buffer.Put ("%es");
            when 2#001# =>
               Buffer.Put ("%cs");
            when 2#010# =>
               Buffer.Put ("%ss");
            when 2#011# =>
               Buffer.Put ("%ds");
            when 2#100# =>
               Buffer.Put ("%fs");
            when 2#101# =>
               Buffer.Put ("%gs");
            when 2#110# =>
               Buffer.Put ("%??");
            when 2#111# =>
               Buffer.Start_Token (Error);
               Buffer.Put ("%??");
         end case;
      end Add_Reg_Seg;

      -------------
      -- Add_Reg --
      -------------

      procedure Add_Reg (F : Bit_Field_4; R : Reg_Class_Type) is
         type Reg_Name3_Array    is array (Bit_Field_3) of String (1 .. 3);
         type Reg_Name3_Array_16 is array (Bit_Field_4) of String (1 .. 3);
         type Reg_Name4_Array_16 is array (Bit_Field_4) of String (1 .. 4);
         type Reg_Name5_Array    is array (Bit_Field_4) of String (1 .. 5);
         Regs_8 : constant Reg_Name4_Array_16 :=
           ("al  ", "cl  ", "dl  ", "bl  ", "ah  ", "ch  ", "dh  ", "bh  ",
            "r8b ", "r9b ", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b");
         Regs_16 : constant Reg_Name4_Array_16 :=
           ("ax  ", "cx  ", "dx  ", "bx  ", "sp  ", "bp  ", "si  ", "di  ",
            "r8w ", "r9w ", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w");
         Regs_32 : constant Reg_Name4_Array_16 :=
           ("eax ", "ecx ", "edx ", "ebx ", "esp ", "ebp ", "esi ", "edi ",
            "r8d ", "r9d ", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d");
         Regs_64 : constant Reg_Name3_Array_16 :=
           ("rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
            "r8 ", "r9 ", "r10", "r11", "r12", "r13", "r14", "r15");
         Regs_Control : constant Reg_Name4_Array_16 :=
           ("cr0 ", "cr1 ", "cr2 ", "cr3 ", "cr4 ", "cr5 ", "cr6 ", "cr7 ",
            "cr8 ", "cr9 ", "cr10", "cr11", "cr12", "cr13", "cr14", "cr15");
         Regs_Debug : constant Reg_Name4_Array_16 :=
           ("dr0 ", "dr1 ", "dr2 ", "dr3 ", "dr4 ", "dr5 ", "dr6 ", "dr7 ",
            "dr8 ", "dr9 ", "dr10", "dr11", "dr12", "dr13", "dr14", "dr15");
         Regs_MM : constant Reg_Name3_Array :=
           ("mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7");
         Regs_XMM : constant Reg_Name5_Array :=
           ("xmm0 ", "xmm1 ", "xmm2 ", "xmm3 ", "xmm4 ", "xmm5 ", "xmm6 ",
            "xmm7 ",
            "xmm8 ", "xmm9 ", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14",
            "xmm15");
      begin
         Buffer.Start_Token (Register);
         Buffer.Put ('%');
         case R is
            when R_8 =>
               Add_Name (Regs_8 (F));
            when R_16 =>
               Add_Name (Regs_16 (F));
            when R_32 =>
               Add_Name (Regs_32 (F));
            when R_64 =>
               Add_Name (Regs_64 (F));
            when R_Control =>
               Add_Name (Regs_Control (F));
            when R_Debug =>
               Add_Name (Regs_Debug (F));
            when R_MM =>
               Add_Name (Regs_MM (Bit_Field_3 (F)));
            when R_XMM =>
               Add_Name (Regs_XMM (F));
            when R_None =>
               raise Invalid_Insn;
         end case;
      end Add_Reg;

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Off not in Insn_Bin.First .. Insn_Bin.Last then
            raise Bad_Memory;
         end if;
         return Get (Insn_Bin, Off);
      end Mem;

      ------------------
      -- Reg_With_Rex --
      ------------------

      function Reg_With_Rex (Rex_Prefix : Bit; Reg : Bit_Field_3)
         return Bit_Field_4 is
      begin
         return Bit_Field_4 (Rex_Prefix) * 8 + Bit_Field_4 (Reg);
      end Reg_With_Rex;

      -------------
      -- Add_Hex --
      -------------

      procedure Add_Hex
        (Value  : Unsigned_64;
         Signed : Boolean;
         Width  : Width_Type)
      is
         V          : Unsigned_64 := Value;

         type Digit_Type is new Unsigned_8 range 0 .. 15;
         Digits_Set   : constant array (Digit_Type) of Character :=
           "0123456789abcdef";
         Digits_Count : Natural := 0;
         Digits_Arr   : array (1 .. 64 / 4) of Digit_Type;

      begin
         Buffer.Start_Token (Literal);

         if Signed and then V >= 16#8000_0000_0000_0000# then

            --  If Value is negative, set V to the absolute value of
            --  Value.

            Buffer.Put ('-');
            V := not V;
            V := V + 1;
         end if;

         for D of Digits_Arr loop
            D := Digit_Type (V and 16#f#);
            V := Shift_Right (V, 4);
            Digits_Count := Digits_Count + 1;
            exit when V = 0;
         end loop;

         Buffer.Put ("0x");

         if Width /= W_None then
            for D in Digits_Count + 1 .. Natural (Width_Len (Width) * 2) loop
               Buffer.Put ('0');
            end loop;
         end if;

         for D of reverse Digits_Arr (1 .. Digits_Count) loop
            Buffer.Put (Digits_Set (D));
         end loop;
      end Add_Hex;

      ----------------
      -- Decode_Val --
      ----------------

      procedure Decode_Val
        (Off    : Pc_Type;
         Width  : Width_Type;
         Signed : Boolean)
      is
      begin
         Add_Hex
           (Decode_Val (Mem'Unrestricted_Access, Off, Width, Signed),
            Signed,
            Width);
      end Decode_Val;

      ----------------
      -- Decode_Imm --
      ----------------

      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type)
      is
      begin
         Buffer.Start_Token (Literal);
         Buffer.Put ('$');
         Decode_Val (Off, Width, False);
         Off := Off + Width_Len (Width);
      end Decode_Imm;

      -----------------
      -- Decode_Disp --
      -----------------

      procedure Decode_Disp (Off           : Pc_Type;
                             Width         : Width_Type;
                             Lookup_Symbol : Boolean;
                             Offset        : Unsigned_32 := 0)
      is
         Decoded_Width : constant Width_Type :=
           (if Lookup_Symbol
            then (if Is_64bit then W_64 else W_32)
            else Width);
         Signed        : Boolean := True;
         V             : Unsigned_64;
      begin
         V := Decode_Val (Mem'Unrestricted_Access, Off, Width, True)
           + Unsigned_64 (Offset);

         --  Displacements are always signed. At this point, V contains a
         --  sign-extended value. If a symbol lookup has been requested,
         --  what we want is an address, though.

         if Lookup_Symbol then
            V := Unsigned_64 (Truncate_To_Pc_Type (V));
            Signed := False;
         end if;

         Buffer.Start_Token (Literal);
         Add_Hex (V, Signed, Decoded_Width);

         if Lookup_Symbol then
            Sym.Symbolize (Truncate_To_Pc_Type (V), Buffer);
         end if;
      end Decode_Disp;

      ---------------------
      -- Decode_Disp_Rel --
      ---------------------

      procedure Decode_Disp_Rel (Off : in out Pc_Type;
                                 Width : Width_Type) is
         Disp_Off : constant Pc_Type := Off;
      begin
         Off := Off + Width_Len (Width);
         Decode_Disp (Disp_Off, Width, True, Unsigned_32 (Off));
      end Decode_Disp_Rel;

      ----------------
      -- Decode_Sib --
      ----------------

      procedure Decode_Sib (Sib : Byte; B_Mod : Bit_Field_2)
      is
         S : Bit_Field_2;
         I : Bit_Field_3;
         B : Bit_Field_3;

         Reg_Addr_Class : Reg_Class_Type;
         Base_Ext       : constant Bit := Ext_Rex_B (Rex_Prefix);
         Index_Ext      : constant Bit := Ext_Rex_X (Rex_Prefix);
      begin
         S := Ext_Sib_Scale (Sib);
         B := Ext_Sib_Base (Sib);
         I := Ext_Sib_Index (Sib);

         if Is_64bit then
            Reg_Addr_Class := R_64;
         else
            Reg_Addr_Class := R_32;
         end if;

         Buffer.Start_Token (Punctuation);
         Buffer.Put ('(');
         if not (B = 2#101# and then B_Mod = 0) then
            --  Base
            Add_Reg (Reg_With_Rex (Base_Ext, B), Reg_Addr_Class);
            Buffer.Start_Token (Punctuation);
            Buffer.Put (',');
         end if;

         --  Index
         if I /= 2#100# then
            Add_Reg (Reg_With_Rex (Index_Ext, I), Reg_Addr_Class);
         else
            Buffer.Start_Token (Register);
            Buffer.Put ("%eiz");
         end if;

         --  Scale
         case S is
            when 2#00# =>
               Buffer.Start_Token (Punctuation);
               Buffer.Put (',');
               Buffer.Start_Token (Literal);
               Buffer.Put ('1');
            when 2#01# =>
               Buffer.Start_Token (Punctuation);
               Buffer.Put (',');
               Buffer.Start_Token (Literal);
               Buffer.Put ('2');
            when 2#10# =>
               Buffer.Start_Token (Punctuation);
               Buffer.Put (',');
               Buffer.Start_Token (Literal);
               Buffer.Put ('4');
            when 2#11# =>
               Buffer.Start_Token (Punctuation);
               Buffer.Put (',');
               Buffer.Start_Token (Literal);
               Buffer.Put ('8');
         end case;
         Buffer.Start_Token (Punctuation);
         Buffer.Put (')');
      end Decode_Sib;

      ----------------------
      -- Decode_Modrm_Reg --
      ----------------------

      procedure Decode_Modrm_Reg (B : Byte; R : Reg_Class_Type) is
         B_Reg   : constant Bit_Field_3 := Ext_Modrm_Reg (B);
         Reg_Ext : constant Bit         := Ext_Rex_R (Rex_Prefix);
      begin
         Add_Reg (Reg_With_Rex (Reg_Ext, B_Reg), R);
      end Decode_Modrm_Reg;

      ----------------------
      -- Decode_Modrm_Mem --
      ----------------------

      procedure Decode_Modrm_Mem (Off : Pc_Type; R : Reg_Class_Type)
      is
         B, Sib         : Byte;
         B_Mod          : Bit_Field_2;
         B_Rm           : Bit_Field_3;

         Reg_Addr_Class : Reg_Class_Type;
         Reg_Ext        : constant Bit := Ext_Rex_B (Rex_Prefix);
      begin
         B := Mem (Off);
         B_Mod := Ext_Modrm_Mod (B);
         B_Rm := Ext_Modrm_Rm (B);

         if Is_64bit then
            Reg_Addr_Class := R_64;
         else
            Reg_Addr_Class := R_32;
         end if;

         --  First the "mod" field of the ModR/M byte, and then the "r/m" one
         --  determine what the operand can be...
         case B_Mod is

            ---------------------
            -- A memory access --
            ---------------------

            when 2#00# =>
               if B_Rm = 2#100# then
                  --  The address is encoded in the SIB byte
                  Sib := Mem (Off + 1);
                  if Ext_Sib_Base (Sib) = 2#101# then
                     --  And in this case, there is also a 32-bit
                     --  displacement with no base.
                     Decode_Disp (Off + 2, W_32, False);
                  end if;
                  Decode_Sib (Sib, B_Mod);

               elsif B_Rm = 2#101# then
                  --  In 32-bit mode, the address is the following 32-bit
                  --  address...
                  if not Is_64bit then
                     Decode_Disp (Off + 1, W_32, True);

                  else
                     --  In 64-bit mode, we also add RIP to it

                     Decode_Disp (Off + 1, W_32, False);

                     Buffer.Start_Token (Punctuation);
                     Buffer.Put ('(');
                     Buffer.Start_Token (Register);
                     Buffer.Put ("%rip");
                     Buffer.Start_Token (Punctuation);
                     Buffer.Put (')');
                  end if;

               else
                  --  Otherwise, the address lies in a register
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put ('(');
                  Add_Reg (Reg_With_Rex (Reg_Ext, B_Rm), Reg_Addr_Class);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put (')');
               end if;

            when 2#01# =>
               if B_Rm = 2#100# then
                  --  The address encoded in the SIB byte plus a 8-bit
                  --  signed displacement.
                  Decode_Disp (Off + 2, W_8, False);
                  Decode_Sib (Mem (Off + 1), B_Mod);

               else
                  --  Otherwise, the address is the content of a register plus
                  --  a 8-bit signed displacement.
                  Decode_Disp (Off + 1, W_8, False);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put ('(');
                  Add_Reg (Reg_With_Rex (Reg_Ext, B_Rm), Reg_Addr_Class);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put (')');
               end if;

            when 2#10# =>
               if B_Rm = 2#100# then
                  --  The address is encoded in the SIB byte plus a 32-bit
                  --  signed displacement.
                  Decode_Disp (Off + 2, W_32, False);
                  Decode_Sib (Mem (Off + 1), B_Mod);
               else
                  --  Otherwise, the address is the content of a register plus
                  --  a signed 32-bit displacement.
                  Decode_Disp (Off + 1, W_32, False);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put ('(');
                  Add_Reg (Reg_With_Rex (Reg_Ext, B_Rm), Reg_Addr_Class);
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put (')');
               end if;

            -----------------------
            -- A register access --
            -----------------------

            when 2#11# =>
               Add_Reg (Reg_With_Rex (Reg_Ext, B_Rm), R);
         end case;
      end Decode_Modrm_Mem;

      ----------------------
      -- Decode_Modrm_Len --
      ----------------------

      function Decode_Modrm_Len (Off : Pc_Type) return Pc_Type
      is
         B : Byte;
         M_Mod : Bit_Field_2;
         M_Rm : Bit_Field_3;
      begin
         B := Mem (Off);
         M_Mod := Ext_Modrm_Mod (B);
         M_Rm := Ext_Modrm_Rm (B);

         case M_Mod is
            when 2#11# =>
               --  Register
               return 1;

            when 2#10# =>
               if M_Rm = 2#100# then
                  --  SIB + disp32
                  return 1 + 1 + 4;
               else
                  return 1 + 4;
               end if;

            when 2#01# =>
               if M_Rm = 2#100# then
                  --  SIB + disp8
                  return 1 + 1 + 1;
               else
                  return 1 + 1;
               end if;

            when 2#00# =>
               if M_Rm = 2#101# then
                  --  disp32
                  return 1 + 4;

               elsif M_Rm = 2#100# then
                  --  SIB
                  if Ext_Sib_Base (Mem (Off + 1)) = 2#101# then
                     return 1 + 1 + 4;
                  else
                     return 1 + 1;
                  end if;

               else
                  return 1;
               end if;
         end case;
      end Decode_Modrm_Len;

      -----------------
      -- Add_Operand --
      -----------------

      procedure Add_Operand (C : Code_Type;
                             Off_Modrm : Pc_Type;
                             Off_Imm : in out Pc_Type;
                             W : Width_Type)
      is
         R       : constant Reg_Class_Type := To_General (W);
         Reg_Ext : constant Bit            := Ext_Rex_B (Rex_Prefix);
      begin
         case C is
            when C_Reg_Al | C_Reg_Cl | C_Reg_Dl | C_Reg_Bl
               | C_Reg_Ah | C_Reg_Ch | C_Reg_Dh | C_Reg_Bh =>
               Add_Reg (Reg_With_Rex (0, To_Register_Number (C)), R_8);
            when C_Reg_Ax | C_Reg_Cx | C_Reg_Dx | C_Reg_Bx
               | C_Reg_Sp | C_Reg_Bp | C_Reg_Si | C_Reg_Di =>
               Add_Reg (Reg_With_Rex (Reg_Ext, To_Register_Number (C)), R);
            when C_Reg_Cs =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%cs");
            when C_Reg_Ds =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%ds");
            when C_Reg_Es =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%es");
            when C_Reg_Fs =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%fs");
            when C_Reg_Gs =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%gs");
            when C_Reg_Ss =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%ss");
            when C_Regs_Eax_Ecx_Edx =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%eax");
               Add_Comma;
               Buffer.Start_Token (Register);
               Buffer.Put ("%ecx");
               Add_Comma;
               Buffer.Start_Token (Register);
               Buffer.Put ("%edx");
            when C_Ap =>
               declare
                  Off_Seg : Pc_Type := Off_Imm + Width_Len (W);
               begin
                  --  First decode the segment selector (located at the end of
                  --  the instruction), then extract the called procedure
                  --  address.

                  Decode_Imm (Off_Seg, W_16);
                  Add_Comma;
                  Decode_Imm (Off_Imm, W);

                  --  The end of the instruction is the end of the segment
                  --  selector.

                  Off_Imm := Off_Seg;
               end;
            when C_Gv =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R);
            when C_Gb =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_8);
            when C_Gw =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_16);
            when C_Gd =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_32);
            when C_Gy =>
               if W = W_16 then
                  Decode_Modrm_Reg (Mem (Off_Modrm), R_64);
               else
                  Decode_Modrm_Reg (Mem (Off_Modrm), R_32);
               end if;
            when C_Gz =>
               Decode_Modrm_Reg (Mem (Off_Modrm), To_General (To_Z (W)));
            when C_Ep | C_Ev | C_Ey =>
               Decode_Modrm_Mem (Off_Modrm, R);
            when C_Ed | C_Ew =>
               Decode_Modrm_Mem (Off_Modrm, R_32);
            when C_Mq_Uq =>
               Decode_Modrm_Mem (Off_Modrm, R_XMM);
            when C_M | C_Ma | C_Mfs | C_Mfd | C_Mfe | C_Md | C_Mw | C_Mp
               | C_Mpd | C_Mps | C_Mq | C_Mdq | C_Ms | C_Msd | C_Mss =>
               Decode_Modrm_Mem (Off_Modrm, R_None);
            when C_Eb | C_Mb =>
               Decode_Modrm_Mem (Off_Modrm, R_8);
            when C_Ib =>
               Decode_Imm (Off_Imm, W_8);
            when C_Iv =>
               Decode_Imm (Off_Imm, W);
            when C_Iw =>
               Decode_Imm (Off_Imm, W_16);
            when C_Iz =>
               Decode_Imm (Off_Imm, To_Z (W));
            when C_Jz =>
               Decode_Disp_Rel (Off_Imm, To_Z (W));
            when C_Jb =>
               Decode_Disp_Rel (Off_Imm, W_8);
            when C_Ov | C_Ob =>
               --  FIXME: the address-size override prefix (0x67) is supposed
               --  to switch between a 16/32-bit addressing scheme, depending
               --  on the architecture. We do not handle it since its usage is
               --  very rare: does it support worth it?
               if Is_64bit then
                  Decode_Imm (Off_Imm, W_64);
               else
                  Decode_Imm (Off_Imm, W_32);
               end if;
            when C_Yb =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%es");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (":(");
               Buffer.Start_Token (Register);
               Buffer.Put ("%edi");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (')');
            when C_Yv | C_Yz =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%es");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (":(");
               Add_Reg (7, R);
               Buffer.Start_Token (Punctuation);
               Buffer.Put (')');
            when C_Xv =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%ds");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (":(");
               Add_Reg (6, R);
               Buffer.Start_Token (Punctuation);
               Buffer.Put (')');
            when C_Xz =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%ds");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (":(");
               if W = W_16 then
                  Add_Reg (6, R_16);
               else
                  Add_Reg (6, R_32);
               end if;
               Buffer.Start_Token (Punctuation);
               Buffer.Put (')');
            when C_Xb =>
               Buffer.Start_Token (Register);
               Buffer.Put ("%ds");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (":(");
               Buffer.Start_Token (Register);
               Buffer.Put ("%esi");
               Buffer.Start_Token (Punctuation);
               Buffer.Put (')');
            when C_H =>
               Add_Reg_St (Ext_Modrm_Rm (Mem (Off_Modrm)));
            when C_H0 =>
               Add_Reg_St (0);
            when C_Cst_1 =>
               Buffer.Start_Token (Literal);
               Buffer.Put ('1');
            when C_Sw =>
               Add_Reg_Seg (Ext_Modrm_Reg (Mem (Off_Modrm)));
            when C_Fv =>
               null;

            when C_Cd =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_Control);
            when C_Dd =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_Debug);
            when C_Rd =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_32);
            when C_Rv =>
               Decode_Modrm_Mem (Off_Modrm, To_General (W));

            when C_Nq =>
               Add_Reg (Bit_Field_4 (Ext_210 (Mem (Off_Modrm))), R_MM);
            when C_Ux =>
               Add_Reg (Bit_Field_4 (Ext_210 (Mem (Off_Modrm))), R_XMM);
            when C_Pd | C_Pq | C_Pw =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_MM);
            when C_Qd | C_Qdq | C_Qq =>
               Decode_Modrm_Mem (Off_Modrm, R_MM);

            when C_Vd | C_Vdq | C_Vps | C_Vpd | C_Vq | C_Vs | C_Vsd | C_Vss
               | C_Vw | C_Vy | C_Vx =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_XMM);

            when C_Wdq | C_Wps | C_Wpd | C_Wq | C_Wsd | C_Wss | C_Wx =>
               Decode_Modrm_Mem (Off_Modrm, R_XMM);

            when others =>
               raise Invalid_Insn with
                 "invalid code type for an operand: " & Code_Type'Image (C);
         end case;
      end Add_Operand;

      -------------------
      -- Update_Length --
      -------------------

      procedure Update_Length (C : Code_Type;
                               Off_Imm : in out Pc_Type;
                               W : Width_Type) is
      begin
         case C is
            when C_Reg_Bp
              | C_Reg_Ax
              | C_Reg_Dx
              | C_Reg_Cx
              | C_Reg_Bx
              | C_Reg_Si
              | C_Reg_Di
              | C_Reg_Sp
              | C_Reg_Al
              | C_Reg_Bl
              | C_Reg_Cl
              | C_Reg_Dl
              | C_Reg_Ah
              | C_Reg_Bh
              | C_Reg_Ch
              | C_Reg_Dh =>
               return;
            when C_Reg_Cs
              | C_Reg_Ds
              | C_Reg_Es
              | C_Reg_Fs
              | C_Reg_Gs
              | C_Reg_Ss =>
               return;
            when C_Regs_Eax_Ecx_Edx =>
               return;
            when C_Gv | C_Gb | C_Gd | C_Gw | C_Gy | C_Gz =>
               return;
            when C_Ib | C_Jb =>
               Off_Imm := Off_Imm + 1;
            when C_Iw =>
               Off_Imm := Off_Imm + 2;
            when C_Iz | C_Jz =>
               Off_Imm := Off_Imm + Width_Len (To_Z (W));
            when C_Iv =>
               Off_Imm := Off_Imm + Width_Len (W);
            when C_Ov | C_Ob =>
               if Is_64bit then
                  Off_Imm := Off_Imm + Width_Len (W_64);
               else
                  Off_Imm := Off_Imm + Width_Len (W_32);
               end if;
            when C_Ap =>
               if W = W_32 then
                  Off_Imm := Off_Imm + 4 + 2;
               else
                  Off_Imm := Off_Imm + 2 + 2;
               end if;
            when C_Mq_Uq =>
               return;
            when C_M | C_Ma | C_Mfs | C_Mfd | C_Mfe | C_Mb | C_Md | C_Mw
               | C_Mp | C_Mpd | C_Mps | C_Mdq | C_Mq | C_Ms | C_Msd | C_Mss =>
               return;
            when C_Eb | C_Ed | C_Ep | C_Ev | C_Ew | C_Ey =>
               return;
            when C_Yb | C_Yv | C_Yz | C_Xv | C_Xz | C_Xb | C_H | C_H0
               | C_Cst_1 =>
               return;
            when C_Sw =>
               return;
            when C_Fv =>
               return;

            when C_Cd | C_Dd | C_Rd | C_Rv =>
               return;

            when C_Nq | C_Ux | C_Pd | C_Pq | C_Pw | C_Qd | C_Qdq | C_Qq =>
               return;
            when C_Vd | C_Vdq | C_Vps | C_Vpd | C_Vq | C_Vs | C_Vsd | C_Vss
               | C_Vw | C_Vy | C_Vx =>
               return;
            when C_Wdq | C_Wps | C_Wpd | C_Wq | C_Wsd | C_Wss | C_Wx =>
               return;

            when others =>
               raise Invalid_Insn with
                 "invalid code type for an operand: " & Code_Type'Image (C);
         end case;
      end Update_Length;

      Off             : Pc_Type;
      Off_Modrm       : Pc_Type;
      Off_Imm         : Pc_Type;

      B, B1           : Byte;
      Prefix_Lock     : Boolean := False;
      Prefix_Oper     : Boolean := False;
      Mandatory_Oper  : Boolean := False;
      Prefix_Repne    : Boolean := False;
      Mandatory_Repne : Boolean := False;
      Prefix_Rep      : Boolean := False;
      Mandatory_Rep   : Boolean := False;
      Use_Modrm       : Boolean := False;

      Desc            : Insn_Desc_Type;
      Name            : String16;
      W               : Width_Type := W_32;
      Seg             : Code_Type := C_None;
      Src, Dst        : Code_Type;
      Extra           : Extra_Operand_Type;

   --  Start of processing for Disassemble_Insn

   begin
      Off := Insn_Bin.First;
      Buffer.Start_Token (Text);

      --  Read the first instruction byte and handle prefixes

      loop
         B := Mem (Off);
         Desc := Insn_Desc (B);
         Off := Off + 1;

         case Desc.Dst is

            --  Consume prefixes and enable corresponding flags

            when C_Prefix_Lock =>
               Prefix_Lock := True;
            when C_Prefix_Oper =>
               Prefix_Oper := True;
            when C_Prefix_Addr =>
               raise Unhandled_Insn; --  TODO???
            when C_Prefix_Rep =>
               Prefix_Rep := True;
            when C_Prefix_Repne =>
               Prefix_Repne := True;

            when C_0F =>
               --  Switch the the 2-byte opcodes

               B   := Mem (Off);
               Off := Off + 1;

               --  Use another lookup table for mandatory prefixes... except
               --  when the slot is empty.

               if Prefix_Oper
                     and then
                  Insn_Desc_66_0F (B).Name (1) /= ' '
               then
                  Desc := Insn_Desc_66_0F (B);
                  Mandatory_Oper := True;
               elsif Prefix_Repne
                        and then
                     Insn_Desc_F2_0F (B).Name (1) /= ' '
               then
                  Desc := Insn_Desc_F2_0F (B);
                  Mandatory_Repne := True;
               elsif Prefix_Rep
                        and then
                     Insn_Desc_F3_0F (B).Name (1) /= ' '
               then
                  Desc := Insn_Desc_F3_0F (B);
                  Mandatory_Rep := True;
               else
                  Desc := Insn_Desc_0F (B);
               end if;
               exit;

            when C_Prefix_Cs .. C_Prefix_Gs =>
               Seg := Desc.Dst;

            when others =>
               if Is_64bit then
                  --  x86_64 particularities are handled here not to disturb
                  --  32-bit disassembling.

                  if (B and 16#f0#) = 16#40# then
                     --  Handle the REX prefixes

                     Rex_Prefix := B;
                     if Ext_Rex_W (Rex_Prefix) = 1 then
                        W := W_64;
                     end if;

                  elsif (B and 16#f0#) = 16#50# then
                     --  Force 64-bit GPR for PUSH/POP
                     W := W_64;
                     exit;

                  elsif B = 16#63# then
                     --  ARPL/MOVSXD is one of the few opcodes  that is
                     --  valid and different instructions in 32/64bit
                     Desc := (Name  => "movsxd          ",
                              Dst   => C_Ev,
                              Src   => C_Gv,
                              Extra => Extra_None);
                     exit;

                  --  For all other cases, just follow the regular decoding

                  else
                     exit;
                  end if;
               else
                  exit;
               end if;
         end case;
      end loop;

      --  Process prefixes that are not mandatory

      if Prefix_Lock then
         Buffer.Start_Token (Prefix);
         Add_Name ("lock            ");
         Buffer.Start_Token (Text);
         Buffer.Put (' ');
      end if;
      if Prefix_Oper and then not Mandatory_Oper and then W = W_32 then
         W := W_16;
      end if;
      if Prefix_Rep and then not Mandatory_Rep then
         Buffer.Start_Token (Prefix);
         Add_Name ("rep             ");
         Buffer.Start_Token (Text);
         Buffer.Put (' ');
      end if;
      if Prefix_Repne and then not Mandatory_Repne then
         Buffer.Start_Token (Prefix);
         Add_Name ("repne           ");
         Buffer.Start_Token (Text);
         Buffer.Put (' ');
      end if;

      --  Do further lookup for special instruction groups (group 1, group 2,
      --  ..., escape to coprocessor instructions), prepare invalid
      --  instructions for output and export mnemonic and operands information
      --  to the proper local variables.

      case Desc.Name (1) is
         when ' ' =>
            null;

         when '1' =>
            B1 := Mem (Off);
            case Desc.Name (2) is
               when ' ' =>
                  Desc.Name (1 .. 3) := Group_Name_1 (Ext_543 (B1));
                  Desc.Name (4)      := ' ';

               when '2' .. '4' =>
                  if Ext_76 (B1) = 2#11# then
                     if Prefix_Oper then
                        Desc := Insn_Desc_G12_13_14_mod11_66
                          (Desc.Name (2), Ext_543 (B1));
                     else
                        Desc := Insn_Desc_G12_13_14_mod11
                          (Desc.Name (2), Ext_543 (B1));
                     end if;
                  else
                     Desc := Invalid_Desc;
                  end if;

               when '5' =>
                  if Ext_76 (B1) = 2#11# then
                     Desc := Insn_Desc_G15_mod11 (Ext_543 (B1));
                  else
                     Desc := Insn_Desc_G15 (Ext_543 (B1));
                  end if;

               when '6' =>
                  Desc := Insn_Desc_G16 (Ext_543 (B1));

               when others =>
                  raise Invalid_Insn;

            end case;
            Use_Modrm     := True;

         when '2' =>
            B1 := Mem (Off);
            Desc.Name (1 .. 3) := Group_Name_2 (Ext_543 (B1));
            Desc.Name (4)      := ' ';
            Use_Modrm     := True;

         when '3' =>
            B1 := Mem (Off);

            --  The instruction inherits its "group" destination.  The first
            --  byte (F6/F7) determines what (instruction dest/src) will be the
            --  src.

            Dst  := Desc.Dst;
            Desc := Insn_Desc_G3 (Ext_543 (B1));
            if B = 16#f6# then
               Desc.Src := Desc.Dst;
            end if;
            Desc.Dst := Dst;

            Use_Modrm := True;

         when '4' =>
            B1        := Mem (Off);
            Desc      := Insn_Desc_G4 (Ext_543 (B1));
            Use_Modrm := True;

         when '5' =>
            B1        := Mem (Off);
            Desc      := Insn_Desc_G5 (Ext_543 (B1));
            Use_Modrm := True;

         when '6' =>
            B1        := Mem (Off);
            Desc      := Insn_Desc_G6 (Ext_543 (B1));
            Use_Modrm := True;

         when '7' =>
            B1 := Mem (Off);
            if Ext_76 (B1) = 2#11#
                 and then
               2#000# <= Ext_543 (B1) and then Ext_543 (B1) <= 2#010#
            then
               case Ext_543 (B1) is
                  when 2#000# =>
                     Desc := Insn_Desc_G7_000_mod11 (Ext_210 (B1));
                  when 2#001# =>
                     Desc := Insn_Desc_G7_001_mod11 (Ext_210 (B1));
                  when 2#010# =>
                     Desc := Insn_Desc_G7_010_mod11 (Ext_210 (B1));
                  when others =>
                     --  Thanks to the enclosing IF statement, excecution flow
                     --  cannot end up here.

                     raise Invalid_Insn;
               end case;
            else
               Desc := Insn_Desc_G7 (Ext_543 (B1));
            end if;
            Use_Modrm := True;

         when '8' =>
            B1        := Mem (Off);
            Desc      := Insn_Desc_G8 (Ext_543 (B1));
            Use_Modrm := True;

         when '9' =>
            B1        := Mem (Off);
            if Ext_76 (B1) = 2#11# then
               Desc := Insn_Desc_G9_mod11 (Ext_543 (B1));
            elsif Prefix_Oper then
               Desc := Insn_Desc_G9_mem_66 (Ext_543 (B1));
            elsif Prefix_Rep then
               Desc := Insn_Desc_G9_mem_F3 (Ext_543 (B1));
            else
               Desc := Insn_Desc_G9_mem (Ext_543 (B1));
            end if;
            Use_Modrm := True;

         when 'E' =>
            B1 := Mem (Off);
            Use_Modrm := True;
            if Ext_Modrm_Mod (B1) /= 2#11# then
               Desc := Insn_Desc_Esc_Before_Bf
                         (Bit_Field_3 (B and 2#111#),
                          Ext_Modrm_Reg (B1));
            else
               Desc := Insn_Desc_Esc_After_Bf
                         (Bit_Field_3 (B and 2#111#),
                          Esc_Outside_Modrm_Type (B1));
            end if;
            Use_Modrm := True;

         when 'a' .. 'z' =>
            null;

         when others =>
            raise Invalid_Insn with "invalid instruction name: " & Desc.Name;
      end case;

      if Desc.Name (1) = ' ' then
         Desc := Invalid_Desc;
      end if;

      Name  := Desc.Name;
      Src   := Desc.Src;
      Dst   := Desc.Dst;
      Extra := Desc.Extra;

      --  Update the ModR/M byte offset and the immediate bytes sequence
      --  offset.

      Off_Modrm := Off;
      if Use_Modrm or else Src in Modrm_Code or else Dst in Modrm_Code then
         Off_Imm := Off_Modrm + Decode_Modrm_Len (Off_Modrm);
      else
         Off_Imm := Off_Modrm;
      end if;

      if Buffer.Length > 0 then
         --  If the caller expects a disassembly text output, append the
         --  mnemonic and instruction operands.

         Buffer.Start_Token (Mnemonic);
         Add_Name (Name);
         Name_Align (1);

         case Extra is
            when Extra_None =>
               null;
            when Extra_8 =>
               Add_Operand (C_Ib, Off_Modrm, Off_Imm, W_8);
               Add_Comma;
            when Extra_Iz =>
               Add_Operand (C_Iz, Off_Modrm, Off_Imm, To_Z (W));
               Add_Comma;
            when Extra_Cl =>
               Add_Reg (2#001#, R_8);
               Add_Comma;
         end case;
         if Src /= C_None then
            case Seg is
               when C_Prefix_Cs | C_Prefix_Ss | C_Prefix_Ds
                  | C_Prefix_Es | C_Prefix_Fs | C_Prefix_Gs =>
                  Add_Reg_Seg (To_Register_Segment (Seg));
                  Buffer.Start_Token (Punctuation);
                  Buffer.Put (":");
               when C_None =>
                  null;
               when others =>
                  raise Invalid_Insn;
            end case;
            Add_Operand (Src, Off_Modrm, Off_Imm, W);
            Add_Comma;
         end if;
         if Dst /= C_None then
            Add_Operand (Dst, Off_Modrm, Off_Imm, W);
         end if;
      else
         --  Otherwise, just put the offset of the first byte after the
         --  instruction into "Off_Imm".

         case Extra is
            when Extra_None | Extra_Cl =>
               null;
            when Extra_8 =>
               Update_Length (C_Ib, Off_Imm, W_8);
            when Extra_Iz =>
               Update_Length (C_Iz, Off_Imm, To_Z (W));
         end case;
         if Src /= C_None then
            Update_Length (Src, Off_Imm, W);
         end if;
         if Dst /= C_None then
            Update_Length (Dst, Off_Imm, W);
         end if;
      end if;

      Insn_Len := Natural (Off_Imm - Insn_Bin.First);

   exception
      when Bad_Memory =>
         Buffer.Start_Token (Error);
         Buffer.Put ("[truncated]");
         Insn_Len := Integer (Length (Insn_Bin));
   end Disassemble_Insn;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      Buffer   : Highlighting.Buffer_Type (0);
      Len      : Natural;

   begin
      Disassemble_Insn
        (Self, Insn_Bin, Insn_Bin.First, Buffer, Len, Nul_Symbolizer);
      return Len;
   end Get_Insn_Length;

   -------------------------
   -- Get_Insn_Properties --
   -------------------------

   procedure Get_Insn_Properties
     (Self        : X86_Disassembler;
      Insn_Bin    : Binary_Content;
      Pc          : Pc_Type;
      Branch      : out Branch_Kind;
      Flag_Indir  : out Boolean;
      Flag_Cond   : out Boolean;
      Branch_Dest : out Dest;
      FT_Dest     : out Dest)
   is
      pragma Unreferenced (Self);

      Is_64bit   : constant Boolean := Machine = Elf_Common.EM_X86_64;

      Opcode_Off : Pc_Type := 0;
      B, B1      : Byte;

      function Mem (Off : Pc_Type) return Byte;

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Opcode_Off + Off > Length (Insn_Bin)  then
            raise Bad_Memory;
         end if;
         return Get (Insn_Bin, Insn_Bin.First + Opcode_Off + Off);
      end Mem;

   --  Start of processing for Get_Insn_Properties

   begin
      --  Make sure OUT parameters have a valid value

      Branch      := Br_None;
      Flag_Indir  := False;
      Flag_Cond   := False;
      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);

      B := Get (Insn_Bin, Insn_Bin.First);

      --  Discard any REX prefix in 64-bit mode and REP/REPNE ones

      while (Is_64bit and then (B and 16#f0#) = 16#40#)
            or else
         (B = 16#f2# or else B = 16#f3#)
      loop
         Opcode_Off := Opcode_Off + 1;
         B := Get (Insn_Bin, Insn_Bin.First + Opcode_Off);
      end loop;

      case B is
         when 16#70# .. 16#7f#
           | 16#e0# .. 16#e2#
           | 16#e3# =>
            --  Jcc Jb / Loop Jb / jrcxz
            Branch     := Br_Jmp;
            Flag_Cond  := True;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 2;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_8, True));
            return;

         when 16#0f# =>
            B := Get (Insn_Bin, Insn_Bin.First + 1);
            if B in 16#80# .. 16#8f# then
               --  Jcc Jz
               Branch     := Br_Jmp;
               Flag_Cond  := True;
               Flag_Indir := False;
               FT_Dest.Target := Pc + 6;
               Branch_Dest.Target :=
                 FT_Dest.Target
                   + Truncate_To_Pc_Type
                 (Decode_Val (Mem'Unrestricted_Access, 2, W_32, True));
            end if;
            return;

         when 16#c2# --  ret
           | 16#c3#
           | 16#ca#  --  retf
           | 16#cb#
           | 16#cf# =>  -- iret
            Branch     := Br_Ret;
            Flag_Cond  := False;
            Flag_Indir := True;
            return;

         when 16#e8# =>
            --  Call near, relative (32-bit offset in both 32-bit and 64-bit
            --  mode).
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_32, True));
            return;

         when 16#9a# =>
            --  Callf, doesn't exist in 64-bit
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Truncate_To_Pc_Type
                (Decode_Val (Mem'Unrestricted_Access, 1, W_32, False));
            return;

         when 16#e9# =>
            --  jmp rel32
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_32, True));
            return;

         when 16#ea# =>
            --  jmp ptr32, doesn't exist in 64-bit
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Truncate_To_Pc_Type
                (Decode_Val (Mem'Unrestricted_Access, 1, W_32, False));
            return;

         when 16#eb# =>
            --  jmp rel8
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 2;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Truncate_To_Pc_Type
              (Decode_Val (Mem'Unrestricted_Access, 1, W_8, True));
            return;

         when 16#ff# =>
            B1 := Get (Insn_Bin, Insn_Bin.First + 1);
            case Ext_543 (B1) is
               when 2#010# | 2#011# =>
                  --  call / callf, absolute indirect
                  Branch     := Br_Call;
                  Flag_Cond  := False;
                  Flag_Indir := True;
                  return;

               when 2#100# | 2#101# =>
                  --  jmp / jmpf, absolute indirect
                  Branch     := Br_Jmp;
                  Flag_Cond  := False;
                  Flag_Indir := True;
                  return;

               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;

   exception
      when Bad_Memory =>
         Warn ("assembler analysis truncated at PC = " & Hex_Image (Pc));
   end Get_Insn_Properties;

end Disa_X86;
