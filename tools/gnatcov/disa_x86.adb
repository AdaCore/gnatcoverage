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

with Interfaces; use Interfaces;
with Outputs;    use Outputs;
with Hex_Images; use Hex_Images;

package body Disa_X86 is

   subtype Byte is Interfaces.Unsigned_8;
   type Bit_Field_2 is mod 2 ** 2;
   type Bit_Field_3 is mod 2 ** 3;

   type Width_Type is (W_None, W_8, W_16, W_32, W_64, W_128);
   --  Width for operands, addresses and registers

   type Reg_Class_Type is
     (R_None,
      R_8, R_16, R_32,
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
      C_Prefix_Rep,
      C_Prefix_Oper,
      C_Prefix_Addr,
      C_0F,
      C_Lock,

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

      --  Either a GPR or a memory address in the "r/m" field of the ModR/M
      --  byte.

      C_Eb,
      C_Ed,
      C_Ep,
      C_Ev,
      C_Ew,

      --  Control, Debug and general Registers in the "reg" field of the ModR/M
      --  byte.

      C_Cd,
      C_Dd,
      C_Rd, --  The "mod" field may refer only to a GPR

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

      --  128-bit XMM register or a memory address

      C_Wdq,
      C_Wps,
      C_Wpd,
      C_Wq,
      C_Wsd,
      C_Wss,

      --  Coprocessor stack register

      C_H0, --  st(0)
      C_H,  --  st(X)

      --  Memory reference to a floating point value

      C_Mfs, --  Single-precision (32 bits)
      C_Mfd, --  Double-precision (64 bits)
      C_Mfe, --  Extended-precision (80 bits)

      C_Sw, --  Segment register in the "reg" field of the ModR/M byte

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

      --  16-bit or 32-bit register, depending on the operand-size attribute

      C_Reg_Ax,
      C_Reg_Cx,
      C_Reg_Bx,
      C_Reg_Dx,
      C_Reg_Sp,
      C_Reg_Bp,
      C_Reg_Si,
      C_Reg_Di,

      --  Segment register

      C_Reg_Es,
      C_Reg_Ss,
      C_Reg_Cs,
      C_Reg_Ds,

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

   type Insn_Desc_Array_Type is array (Byte) of Insn_Desc_Type;
   --  Lookup table kind for one-byte and two-bytes opcode sequences

   type Group_Desc_Array_Type is array (Bit_Field_3) of Insn_Desc_Type;
   --  Lookup table kind for ModR/M-extended opcodes

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
      16#f0#        => ("lock            ", C_Lock, C_None, Extra_None),
      16#f1#        => ("                ", C_None, C_None, Extra_None),
      16#f2#        => ("repne           ", C_Prefix_Rep, C_None, Extra_None),
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
      16#12#        => ("movlps          ", C_Vq, C_Mq, Extra_None),
      16#13#        => ("movlps          ", C_Mq, C_Vq, Extra_None),
      16#14#        => ("unpcklps        ", C_Vs, C_Wps, Extra_None),
      16#15#        => ("unpckhps        ", C_Vs, C_Wps, Extra_None),
      16#16#        => ("movhps          ", C_Vq, C_Mq, Extra_None),
      16#17#        => ("movhps          ", C_Mq, C_Vps, Extra_None),

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
      16#2c#        => ("cvttps2pi       ", C_Pq,  C_Wq, Extra_None),
      16#2d#        => ("cvtps2pi        ", C_Pq,  C_Wq, Extra_None),
      16#2e#        => ("ucomiss         ", C_Vss, C_Wss, Extra_None),
      16#2f#        => ("comiss          ", C_Vps, C_Wps, Extra_None),

      16#30#        => ("wrmsr           ", C_None, C_None, Extra_None),
      16#31#        => ("rdtsc           ", C_None, C_None, Extra_None),
      16#32#        => ("rdmsr           ", C_None, C_None, Extra_None),
      16#33#        => ("rdpmc           ", C_None, C_None, Extra_None),
      16#34#        => ("sysenter        ", C_None, C_None, Extra_None),
      16#35#        => ("sysexit         ", C_None, C_None, Extra_None),
      --  The 16#36#-16#3f# slot are reserved

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
      --  TODO??? 12/13/14 extended opcodes forms
      16#74#        => ("pcmpeqb         ", C_Pq, C_Qq, Extra_None),
      16#75#        => ("pcmpeqw         ", C_Pq, C_Qq, Extra_None),
      16#76#        => ("pcmepeqd        ", C_Pq, C_Qq, Extra_None),
      16#77#        => ("emms            ", C_None, C_None, Extra_None),
      --  The 16#78#-16#7b# slots are reserved
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

      16#a4#        => ("shld            ", C_Ev, C_Gv, Extra_8),
      16#a5#        => ("shld            ", C_Ev, C_Gv, Extra_Cl),
      16#ac#        => ("shrd            ", C_Ev, C_Gv, Extra_8),
      16#ad#        => ("shrd            ", C_Ev, C_Gv, Extra_Cl),
      16#af#        => ("imul            ", C_Gv, C_Ev, Extra_None),

      16#b6#        => ("movzx           ", C_Gv, C_Eb, Extra_None),
      16#b7#        => ("movzx           ", C_Gv, C_Ew, Extra_None),
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
      --  TODO??? 9 extended opcodes forms
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
      16#d7#        => ("pmovmskb        ", C_Gd, C_Pq, Extra_None),
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
      16#f7#        => ("maskmovq        ", C_Pq, C_Pq, Extra_None),
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
      16#16#        => ("movhpd          ", C_Vq, C_Mq, Extra_None),
      16#17#        => ("movhpd          ", C_Mq, C_Vpd, Extra_None),

      16#28#        => ("movapd          ", C_Vpd, C_Wpd, Extra_None),
      16#29#        => ("movapd          ", C_Wpd, C_Vpd, Extra_None),
      16#2a#        => ("cvtpi2pd        ", C_Vpd, C_Qd, Extra_None),
      16#2b#        => ("movntpd         ", C_Mpd, C_Vpd, Extra_None),
      16#2c#        => ("cvttpd2pi       ", C_Pq, C_Wpd, Extra_None),
      16#2d#        => ("cptpd2pi        ", C_Pq, C_Wpd, Extra_None),
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
      --  TODO??? 12/13/14 extended opcodes forms
      16#74#        => ("pcmpeqb         ", C_Vdq, C_Wdq, Extra_8),
      16#75#        => ("pcmpeqw         ", C_Vdq, C_Wdq, Extra_8),
      16#76#        => ("pcmpeqd         ", C_Vdq, C_Wdq, Extra_8),
      --  The 16#77#-16#7b# slots are reserved
      16#7c#        => ("haddpd          ", C_Vdq, C_Wdq, Extra_8),
      16#7d#        => ("hsubpd          ", C_Vdq, C_Wdq, Extra_8),
      16#7e#        => ("movd            ", C_Ed, C_Vd, Extra_8),
      16#7f#        => ("movdqa          ", C_Wdq, C_Vdq, Extra_8),

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
      16#12#        => ("movddup         ", C_Vq, C_Wq, Extra_None),
      16#1a#        => ("cvtsi2sd        ", C_Vsd, C_Ed, Extra_None),
      16#1c#        => ("cvttsd2si       ", C_Gd, C_Wsd, Extra_None),
      16#1d#        => ("cvtsd2si        ", C_Gd, C_Wsd, Extra_None),

      --  Here...
      16#52#        => ("sqrtsdsi        ", C_Vsd, C_Wsd, Extra_None),
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
      16#13#        => ("movsldup        ", C_Vps, C_Wps, Extra_None),
      --  The 16#14#-16#15# slots are reserved
      16#16#        => ("movshdup        ", C_Vps, C_Wps, Extra_None),
      --  The 16#17# slot is reserved
      --  TODO??? 16 extended opcodes forms
      --  The 16#19#-16#1f# slots are reserved

      16#2a#        => ("cvtsi2ss        ", C_Vss, C_Ed, Extra_None),
      16#2c#        => ("cvttss2si       ", C_Gd, C_Wss, Extra_None),
      16#2d#        => ("cvtss2si        ", C_Gd, C_Wss, Extra_None),

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

   --  Two-levels lookup table for escape to coprocessor instruction set, when
   --  the ModR/M byte is inside the range from 16#00# to 16#bf# (included). It
   --  is indexed first by the 3 least significant bits of the first opcode
   --  byte, then by the "reg/opcode" 3-bit field of the ModR/M byte.

   type Esc_Inside_Desc_Array_Type is
      array (Bit_Field_3, Bit_Field_3) of Insn_Desc_Type;

   Insn_Desc_Esc_Before_Bf : constant Esc_Inside_Desc_Array_Type :=
     (
      --  D8: all slots are reserved
      (others => ("                ", C_None, C_None, Extra_None)),
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
      W_64   => R_None,
      W_128  => R_None);
   --  Turn an operand size into the corresponding general-purpose register
   --  class (if any, R_None otherwise).

   type To_Z_Type is array (Width_Type) of Width_Type;
   To_Z : constant To_Z_Type :=
     (W_None => W_None,
      W_8    => W_None,
      W_16   => W_16,
      W_32   => W_32,
      W_64   => W_None,
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
     (Mem   : Mem_Read;
      Off   : Pc_Type;
      Width : Width_Type)
     return Unsigned_32;
   --  Decode an immediate (unsigned) value given its memory location and its
   --  size.
   --  Off is the immediate address and is relative to a certain PC. Mem is a
   --  function that reads one byte at an offset from this PC.

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

   type Hex_Str is array (Natural range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Str := "0123456789abcdef";

   ----------------
   -- Decode_Val --
   ----------------

   function Decode_Val
     (Mem   : Mem_Read;
      Off   : Pc_Type;
      Width : Width_Type)
     return Unsigned_32
   is
      V : Unsigned_32;
   begin
      case Width is
         when W_8 =>
            V := Unsigned_32 (Mem (Off));
            --  Sign extension

            if V >= 16#80# then
               V := 16#ffff_ff00# or V;
            end if;
            return V;

         when W_16 =>
            return Shift_Left (Unsigned_32 (Mem (Off + 1)), 8)
              or Unsigned_32 (Mem (Off));

         when W_32 =>
            return  Shift_Left (Unsigned_32 (Mem (Off + 3)), 24)
              or Shift_Left (Unsigned_32 (Mem (Off + 2)), 16)
              or Shift_Left (Unsigned_32 (Mem (Off + 1)), 8)
              or Shift_Left (Unsigned_32 (Mem (Off + 0)), 0);

         when W_64 | W_128 =>
            raise Program_Error with "unhandled 64/128bits decoding";

         when W_None =>
            raise Program_Error;
      end case;
   end Decode_Val;

   ----------------------
   -- Disassemble_Insn --
   ----------------------

   procedure Disassemble_Insn
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content;
      Pc       : Pc_Type;
      Line     : out String;
      Line_Pos : out Natural;
      Insn_Len : out Natural;
      Sym      : Symbolizer'Class)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Pc);

      Lo : Natural;
      --  Index in LINE of the next character to be written

      function Mem (Off : Pc_Type) return Byte;
      --  The instruction memory, 0 based

      ----------------------------
      -- Basic output utilities --
      ----------------------------

      --  The following functions add basic elements (a character, an
      --  instruction mnemonic, a byte, etc.) to the output line.

      procedure Add_Name (Name : String16);
      pragma Inline (Add_Name);

      procedure Add_Char (C : Character);
      pragma Inline (Add_Char);

      procedure Add_String (Str : String);
      procedure Add_Byte (V : Byte);
      procedure Add_Comma;

      procedure Name_Align (Orig : Natural);
      --  Pad the current line with blanks to align operands after the
      --  instruction mnemonic.

      procedure Add_Reg (F : Bit_Field_3; R : Reg_Class_Type);
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

      procedure Decode_Val (Off : Pc_Type; Width : Width_Type);
      --  Add the value in the binary at the given "Off" offset of the given
      --  "Width" to the output.

      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type);
      --  Add the value in the binary at the given "Off" offset of the given
      --  "Width" to the output as an assembly immediate (with the "$0x"
      --  prefix) and update "Off" to point to the first byte past the
      --  immediate.

      procedure Decode_Disp (Off : Pc_Type;
                             Width : Width_Type;
                             Offset : Unsigned_32 := 0);
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
      -- Add_Char --
      --------------

      procedure Add_Char (C : Character) is
      begin
         if Lo <= Line'Last then
            Line (Lo) := C;
            Lo := Lo + 1;
         end if;
      end Add_Char;

      ----------------
      -- Add_String --
      ----------------

      procedure Add_String (Str : String) is
      begin
         if Lo + Str'Length <= Line'Last then
            Line (Lo .. Lo + Str'Length - 1) := Str;
            Lo := Lo + Str'Length;
         else
            for I in Str'Range loop
               Add_Char (Str (I));
            end loop;
         end if;
      end Add_String;

      --------------
      -- Add_Byte --
      --------------

      procedure Add_Byte (V : Byte) is
      begin
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 4) and 16#0f#)));
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 0) and 16#0f#)));
      end Add_Byte;

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name (Name : String16) is
      begin
         for I in Name'Range loop
            exit when Name (I) = ' ';
            Add_Char (Name (I));
         end loop;
      end Add_Name;

      ---------------
      -- Add_Comma --
      ---------------

      procedure Add_Comma is
      begin
         Add_String (", ");
      end Add_Comma;

      ----------------
      -- Name_Align --
      ----------------

      procedure Name_Align (Orig : Natural) is
      begin
         Add_Char (' ');
         while Lo - Orig < 8 loop
            Add_Char (' ');
         end loop;
      end Name_Align;

      ----------------
      -- Add_Opcode --
      ----------------

      procedure Add_Opcode (Name : String16; Width : Width_Type) is
         L : constant Natural := Lo;
      begin
         Add_Name (Name);
         if False and Width /= W_None then
            Add_Char (Width_Char (Width));
         end if;
         Name_Align (L);
      end Add_Opcode;

      ----------------
      -- Add_Reg_St --
      ----------------

      procedure Add_Reg_St (F : Bit_Field_3) is
      begin
         Add_String ("%st(");
         Add_Char (Hex_Digit (Natural (F)));
         Add_Char (')');
      end Add_Reg_St;

      -----------------
      -- Add_Reg_Seg --
      -----------------

      procedure Add_Reg_Seg (F : Bit_Field_3) is
      begin
         case F is
            when 2#000# =>
               Add_String ("%es");
            when 2#001# =>
               Add_String ("%cs");
            when 2#010# =>
               Add_String ("%ss");
            when 2#011# =>
               Add_String ("%ds");
            when 2#100# =>
               Add_String ("%fs");
            when 2#101# =>
               Add_String ("%gs");
            when 2#110# =>
               Add_String ("%??");
            when 2#111# =>
               Add_String ("%??");
         end case;
      end Add_Reg_Seg;

      -------------
      -- Add_Reg --
      -------------

      procedure Add_Reg (F : Bit_Field_3; R : Reg_Class_Type) is
         type Reg_Name2_Array is array (Bit_Field_3) of String (1 .. 2);
         type Reg_Name3_Array is array (Bit_Field_3) of String (1 .. 3);
         type Reg_Name4_Array is array (Bit_Field_3) of String (1 .. 4);
         Regs_8 : constant Reg_Name2_Array :=
           ("al", "cl", "dl", "bl", "ah", "ch", "dh", "bh");
         Regs_16 : constant Reg_Name2_Array :=
           ("ax", "cx", "dx", "bx", "sp", "bp", "si", "di");
         Regs_32 : constant Reg_Name3_Array :=
           ("eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi");
         Regs_Control : constant Reg_Name3_Array :=
           ("cr0", "cr1", "cr2", "cr3", "cr4", "cr5", "cr6", "cr7");
         Regs_Debug : constant Reg_Name3_Array :=
           ("dr0", "dr1", "dr2", "dr3", "dr4", "dr5", "dr6", "dr7");
         Regs_MM : constant Reg_Name3_Array :=
           ("mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7");
         Regs_XMM : constant Reg_Name4_Array :=
           ("xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7");
      begin
         Add_Char ('%');
         case R is
            when R_8 =>
               Add_String (Regs_8 (F));
            when R_16 =>
               Add_String (Regs_16 (F));
            when R_32 =>
               Add_String (Regs_32 (F));
            when R_Control =>
               Add_String (Regs_Control (F));
            when R_Debug =>
               Add_String (Regs_Debug (F));
            when R_MM =>
               Add_String (Regs_MM (F));
            when R_XMM =>
               Add_String (Regs_XMM (F));
            when R_None =>
               raise Program_Error;
         end case;
      end Add_Reg;

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Off not in Insn_Bin'Range then
            raise Bad_Memory;
         end if;
         return Insn_Bin (Off);
      end Mem;

      ----------------
      -- Decode_Val --
      ----------------

      procedure Decode_Val (Off : Pc_Type; Width : Width_Type)
      is
      begin
         case Width is
            when W_8 =>
               Add_Byte (Mem (Off));
            when W_16 =>
               Add_Byte (Mem (Off + 1));
               Add_Byte (Mem (Off));
            when W_32 =>
               Add_Byte (Mem (Off + 3));
               Add_Byte (Mem (Off + 2));
               Add_Byte (Mem (Off + 1));
               Add_Byte (Mem (Off + 0));
            when W_None =>
               raise Program_Error;
            when others =>
               raise Program_Error with "unhandled 64/128 bits decoding";
         end case;
      end Decode_Val;

      ----------------
      -- Decode_Imm --
      ----------------

      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type)
      is
      begin
         Add_String ("$0x");
         Decode_Val (Off, Width);
         Off := Off + Width_Len (Width);
      end Decode_Imm;

      -----------------
      -- Decode_Disp --
      -----------------

      procedure Decode_Disp (Off : Pc_Type;
                             Width : Width_Type;
                             Offset : Unsigned_32 := 0)
      is
         L : Natural;
         V : Unsigned_32;
      begin
         L := Lo;
         V := Decode_Val (Mem'Unrestricted_Access, Off, Width) + Offset;
         Sym.Symbolize (Pc_Type (V), Line, Lo);
         if L /= Lo then
            if V = 0 then
               return;
            end if;
            Add_String (" + ");
         end if;
         Add_String ("0x");
         if Offset = 0 then
            Decode_Val (Off, Width);
         else
            Add_Byte (Byte (Shift_Right (V, 24) and 16#ff#));
            Add_Byte (Byte (Shift_Right (V, 16) and 16#ff#));
            Add_Byte (Byte (Shift_Right (V, 8) and 16#ff#));
            Add_Byte (Byte (Shift_Right (V, 0) and 16#ff#));
         end if;
         --  --  First try to display a symbol associated to the given
         --  --  displacement.
         --  L := Lo;
         --  V := Decode_Val (Mem'Unrestricted_Access, Off, Width) + Offset;
         --  Sym.Symbolize (Pc_Type (V), Line, Lo);

         --  --  If the symbolizer wrote nothing, write the hexadecimal
         --  --  displacement instead.

         --  if L = Lo then
         --     Add_String ("0x");
         --     Decode_Val (Off_Orig, Width);
         --  end if;

         --  --  Prepare the addition for what is displaced.
         --  Add_String (" + ");
      end Decode_Disp;

      ---------------------
      -- Decode_Disp_Rel --
      ---------------------

      procedure Decode_Disp_Rel (Off : in out Pc_Type;
                                 Width : Width_Type) is
         Disp_Off : constant Pc_Type := Off;
      begin
         Off := Off + Width_Len (Width);
         Decode_Disp (Disp_Off, Width, Unsigned_32 (Off));
      end Decode_Disp_Rel;

      ----------------
      -- Decode_Sib --
      ----------------

      procedure Decode_Sib (Sib : Byte; B_Mod : Bit_Field_2)
      is
         S : Bit_Field_2;
         I : Bit_Field_3;
         B : Bit_Field_3;
      begin
         S := Ext_Sib_Scale (Sib);
         B := Ext_Sib_Base (Sib);
         I := Ext_Sib_Index (Sib);
         Add_Char ('(');
         if not (B = 2#101# and then B_Mod = 0) then
            --  Base
            Add_Reg (B, R_32);
            if I /= 2#100# then
               Add_Char (',');
            end if;
         end if;
         if I /= 2#100# then
            --  Index
            Add_Reg (I, R_32);
            --  Scale
            case S is
               when 2#00# =>
                  null;
               when 2#01# =>
                  Add_String (",2");
               when 2#10# =>
                  Add_String (",4");
               when 2#11# =>
                  Add_String (",8");
            end case;
         end if;
         Add_Char (')');
      end Decode_Sib;

      ----------------------
      -- Decode_Modrm_Reg --
      ----------------------

      procedure Decode_Modrm_Reg (B : Byte; R : Reg_Class_Type) is
      begin
         Add_Reg (Ext_Modrm_Reg (B), R);
      end Decode_Modrm_Reg;

      ----------------------
      -- Decode_Modrm_Mem --
      ----------------------

      procedure Decode_Modrm_Mem (Off : Pc_Type; R : Reg_Class_Type)
      is
         B, Sib : Byte;
         B_Mod : Bit_Field_2;
         B_Rm : Bit_Field_3;
      begin
         B := Mem (Off);
         B_Mod := Ext_Modrm_Mod (B);
         B_Rm := Ext_Modrm_Rm (B);

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
                     --  And in this case, there is also a 32-bit displacement
                     --  with no base.
                     Decode_Disp (Off + 2, W_32);
                  end if;
                  Decode_Sib (Sib, B_Mod);

               elsif B_Rm = 2#101# then
                  --  The address is the following 32-bit address
                  Decode_Disp (Off + 1, W_32);

               else
                  --  Otherwise, the address lies in a register
                  Add_Char ('(');
                  Add_Reg (B_Rm, R_32);
                  Add_Char (')');
               end if;

            when 2#01# =>
               if B_Rm = 2#100# then
                  --  The address encoded in the SIB byte plus a 8-bit
                  --  displacement.
                  Decode_Disp (Off + 2, W_8);
                  Decode_Sib (Mem (Off + 1), B_Mod);

               else
                  --  Otherwise, the address is the content of a register plus
                  --  a 8-bit displacement.
                  Decode_Disp (Off + 1, W_8);
                  Add_Char ('(');
                  Add_Reg (B_Rm, R_32);
                  Add_Char (')');
               end if;

            when 2#10# =>
               if B_Rm = 2#100# then
                  --  The address is encoded in the SIB byte plus a 32-bit
                  --  displacement.
                  Decode_Disp (Off + 2, W_32);
                  Decode_Sib (Mem (Off + 1), B_Mod);
               else
                  --  Otherwise, the address is the content of a register plus
                  --  a 32-bit displacement.
                  Decode_Disp (Off + 1, W_32);
                  Add_Char ('(');
                  Add_Reg (B_Rm, R_32);
                  Add_Char (')');
               end if;

            -----------------------
            -- A register access --
            -----------------------

            when 2#11# =>
               Add_Reg (B_Rm, R);
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
         Off2 : Pc_Type;
         R : constant Reg_Class_Type := To_General (W);
      begin
         case C is
            when C_Reg_Al | C_Reg_Cl | C_Reg_Dl | C_Reg_Bl
               | C_Reg_Ah | C_Reg_Ch | C_Reg_Dh | C_Reg_Bh =>
               Add_Reg (To_Register_Number (C), R_8);
            when C_Reg_Ax | C_Reg_Cx | C_Reg_Dx | C_Reg_Bx
               | C_Reg_Sp | C_Reg_Bp | C_Reg_Si | C_Reg_Di =>
               Add_Reg (To_Register_Number (C), R);
            when C_Reg_Cs =>
               Add_String ("%cs");
            when C_Reg_Ds =>
               Add_String ("%ds");
            when C_Reg_Es =>
               Add_String ("%es");
            when C_Reg_Ss =>
               Add_String ("%ss");
            when C_Ap =>
               Off2 := Off_Imm;
               Off_Imm := Off_Imm + 4;  -- FIXME
               Decode_Imm (Off_Imm, W_16);
               Add_Comma;
               Decode_Imm (Off2, W_32);  -- FIXME
            when C_Gv =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R);
            when C_Gb =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_8);
            when C_Ev =>
               Decode_Modrm_Mem (Off_Modrm, R);
            when C_Ed | C_Ew =>
               Decode_Modrm_Mem (Off_Modrm, R_32);
            when C_Ep =>
               Decode_Modrm_Mem (Off_Modrm, R);
            when C_M | C_Mfs | C_Mfd | C_Mfe | C_Md | C_Mpd | C_Mps | C_Mq
               | C_Mdq | C_Ms =>
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
               Decode_Imm (Off_Imm, W_32); --  FIXME: oper16
            when C_Yb =>
               Add_String ("%es:(%edi)");
            when C_Yv | C_Yz =>
               Add_String ("%es:(");
               Add_Reg (7, R);
               Add_Char (')');
            when C_Xv =>
               Add_String ("%ds:(");
               Add_Reg (6, R);
               Add_Char (')');
            when C_Xb =>
               Add_String ("%ds:(%esi)");
            when C_H =>
               Add_Reg_St (Ext_Modrm_Rm (Mem (Off_Modrm)));
            when C_H0 =>
               Add_Reg_St (0);
            when C_Cst_1 =>
               Add_String ("1");
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

            when C_Pd | C_Pq | C_Pw =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_MM);
            when C_Qd | C_Qdq | C_Qq =>
               Decode_Modrm_Mem (Off_Modrm, R_MM);

            when C_Vd | C_Vdq | C_Vps | C_Vpd | C_Vq | C_Vs | C_Vsd | C_Vss
               | C_Vw =>
               Decode_Modrm_Reg (Mem (Off_Modrm), R_XMM);

            when C_Wdq | C_Wps | C_Wpd | C_Wq | C_Wsd | C_Wss =>
               Decode_Modrm_Mem (Off_Modrm, R_XMM);

            when others =>
               raise Program_Error with
                 "operand: unhandled x86 code_type " & Code_Type'Image (C);
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
            when C_Reg_Es
              | C_Reg_Ds
              | C_Reg_Ss
              | C_Reg_Cs =>
               return;
            when C_Gv | C_Gb =>
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
               Off_Imm := Off_Imm + Width_Len (W_32); -- FIXME: oper16
            when C_Ap =>
               Off_Imm := Off_Imm + 4 + 2; -- FIXME: oper16
            when C_M | C_Mfs | C_Mfd | C_Mfe | C_Md | C_Mpd | C_Mps | C_Mdq
               | C_Mq | C_Ms =>
               return;
            when C_Eb | C_Ed | C_Ep | C_Ev | C_Ew =>
               return;
            when C_Yb | C_Yv | C_Yz | C_Xv | C_Xb | C_H | C_H0 | C_Cst_1 =>
               return;
            when C_Sw =>
               return;
            when C_Fv =>
               return;

            when C_Cd | C_Dd | C_Rd =>
               return;

            when C_Pd | C_Pq | C_Pw | C_Qd | C_Qdq | C_Qq =>
               return;
            when C_Vd | C_Vdq | C_Vps | C_Vpd | C_Vq | C_Vs | C_Vsd | C_Vss
               | C_Vw =>
               return;
            when C_Wdq | C_Wps | C_Wpd | C_Wq | C_Wsd | C_Wss =>
               return;

            when others =>
               raise Program_Error with
                 "length: unhandled x86 code_type " & Code_Type'Image (C);
         end case;
      end Update_Length;

      Off       : Pc_Type;
      Off_Modrm : Pc_Type;
      Off_Imm   : Pc_Type;

      B, B1     : Byte;
      Use_Modrm : Boolean := False;

      Desc     : Insn_Desc_Type;
      Name     : String16;
      W        : Width_Type := W_32;
      Seg      : Code_Type := C_None;
      Src, Dst : Code_Type;
      Extra    : Extra_Operand_Type;

   --  Start of processing for Disassemble_Insn

   begin
      Off := Insn_Bin'First;
      Lo := Line'First;

      --  Read the first instruction byte and handle prefixes

      loop
         B := Mem (Off);
         Desc := Insn_Desc (B);
         Off := Off + 1;

         case Desc.Dst is
            when C_Prefix_Rep =>
               B1 := Mem (Off);
               Off := Off + 1;
               if B1 = 16#0f# then
                  B1 := Mem (Off);
                  Off := Off + 1;
                  case B is
                     when 16#f2# =>
                        Desc := Insn_Desc_F2_0F (B1);
                     when 16#f3# =>
                        Desc := Insn_Desc_F3_0F (B1);
                     when others =>
                        Desc.Name (1) := ' ';
                        Desc.Dst := C_None;
                        Desc.Src := C_None;
                  end case;
               else
                  Add_Name (Desc.Name);
                  Add_Char (' ');
                  Desc := Insn_Desc (B1);
               end if;
               exit;

            when C_Lock =>
               Add_Name (Desc.Name);
               Add_Char (' ');

            when C_Prefix_Oper =>
               B1 := Mem (Off);
               if B = 16#66# and then B1 = 16#0f# then
                  Off := Off + 1;
                  B1 := Mem (Off);
                  Off := Off + 1;
                  Desc := Insn_Desc_66_0F (B1);

                  --  When the 16#66# prefix is used but was not mandatory,
                  --  this is a valid operand-size attribute toggling: turn
                  --  back to the unprefixed two-bytes opcodes table.
                  if Desc.Name (1) = ' ' then
                     Desc := Insn_Desc_0F (B);
                  end if;
                  exit;
               end if;
               W := W_16;

            when C_0F =>
               B := Mem (Off);
               Off := Off + 1;
               Desc := Insn_Desc_0F (B);
               exit;

            when C_Prefix_Addr =>
               --  TODO???
               raise Program_Error;

            when C_Prefix_Cs .. C_Prefix_Gs =>
               Seg := Desc.Dst;

            when others =>
               exit;
         end case;
      end loop;

      --  Do further lookup for special instruction groups (group 1, group 2,
      --  ..., escape to coprocessor instructions), prepare invalid
      --  instructions for output and export mnemonic and operands information
      --  to the proper local variables.

      case Desc.Name (1) is
         when ' ' =>
            Name  := "invalid*        ";
            Src   := C_None;
            Dst   := C_None;
            Extra := Extra_None;

         when '1' =>
            B1 := Mem (Off);
            Name (1 .. 3) := Group_Name_1 (Ext_543 (B1));
            Name (4)      := ' ';
            Src           := Desc.Src;
            Dst           := Desc.Dst;
            Extra         := Desc.Extra;

         when '2' =>
            B1 := Mem (Off);
            Name (1 .. 3) := Group_Name_2 (Ext_543 (B1));
            Name (4)      := ' ';
            Src           := Desc.Src;
            Dst           := Desc.Dst;
            Extra         := Desc.Extra;

         when '3' =>
            B1    := Mem (Off);
            Dst   := Desc.Dst;
            Extra := Desc.Extra;
            Desc  := Insn_Desc_G3 (Ext_543 (B1));
            Name  := Desc.Name;

            if B = 16#f6# then
               Src := Desc.Dst;
            else
               Src := Desc.Src;
            end if;

         when '4' =>
            B1    := Mem (Off);
            Desc  := Insn_Desc_G4 (Ext_543 (B1));
            Name  := Desc.Name;
            Src   := Desc.Src;
            Dst   := Desc.Dst;
            Extra := Desc.Extra;

         when '5' =>
            B1    := Mem (Off);
            Desc  := Insn_Desc_G5 (Ext_543 (B1));
            Name  := Desc.Name;
            Src   := Desc.Src;
            Dst   := Desc.Dst;
            Extra := Desc.Extra;

         when '6' =>
            B1    := Mem (Off);
            Desc  := Insn_Desc_G6 (Ext_543 (B1));
            Name  := Desc.Name;
            Src   := Desc.Src;
            Dst   := Desc.Dst;
            Extra := Desc.Extra;

         when '7' =>
            B1 := Mem (Off);
            Desc  := Insn_Desc_G7 (Ext_543 (B1));
            Name  := Desc.Name;
            Src   := Desc.Src;
            Dst   := Desc.Dst;
            Extra := Desc.Extra;

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
            Name  := Desc.Name;
            Dst   := Desc.Dst;
            Src   := Desc.Src;
            Extra := Desc.Extra;

         when 'a' .. 'z' =>
            Name  := Desc.Name;
            Src   := Desc.Src;
            Dst   := Desc.Dst;
            Extra := Desc.Extra;

         when others =>
            raise Program_Error with "disa_x86 unhandled name " & Desc.Name;
      end case;

      --  Update the ModR/M byte offset and the immediate bytes sequence
      --  offset.

      Off_Modrm := Off;
      if Use_Modrm or else Src in Modrm_Code or else Dst in Modrm_Code then
         Off_Imm := Off_Modrm + Decode_Modrm_Len (Off_Modrm);
      else
         Off_Imm := Off_Modrm;
      end if;

      if Line'Length > 0 then
         --  If the caller expects a disassembly text output, append the
         --  mnemonic and instruction operands.

         Add_Name (Name);
         Name_Align (Line'First);

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
                  Add_String (":");
               when C_None =>
                  null;
               when others =>
                  raise Program_Error;
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

      Line_Pos := Lo;
      Insn_Len := Natural (Off_Imm - Insn_Bin'First);

   exception
      when Bad_Memory =>
         Add_String ("[truncated]");
         Line_Pos := Lo;
         Insn_Len := Insn_Bin'Length;
   end Disassemble_Insn;

   ---------------------
   -- Get_Insn_Length --
   ---------------------

   function Get_Insn_Length
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content) return Positive
   is
      Line     : String (1 .. 0);
      Line_Pos : Natural;
      Len      : Natural;

   begin
      Disassemble_Insn
        (Self, Insn_Bin, Insn_Bin'First, Line, Line_Pos, Len, Nul_Symbolizer);
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

      B, B1 : Byte;

      function Mem (Off : Pc_Type) return Byte;

      ---------
      -- Mem --
      ---------

      function Mem (Off : Pc_Type) return Byte is
      begin
         if Off > Insn_Bin'Length  then
            raise Bad_Memory;
         end if;
         return Insn_Bin (Insn_Bin'First + Off);
      end Mem;

   --  Start of processing for Get_Insn_Properties

   begin
      --  Make sure OUT parameters have a valid value

      Branch_Dest := (No_PC, No_PC);
      FT_Dest     := (No_PC, No_PC);
      Branch      := Br_None;

      B := Insn_Bin (Insn_Bin'First);

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
                + Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_8));
            return;

         when 16#0f# =>
            B := Insn_Bin (Insn_Bin'First + 1);
            if B in 16#80# .. 16#8f# then
               --  Jcc Jz
               Branch     := Br_Jmp;
               Flag_Cond  := True;
               Flag_Indir := False;
               FT_Dest.Target := Pc + 6;
               Branch_Dest.Target :=
                 FT_Dest.Target
                   + Pc_Type (Decode_Val (Mem'Unrestricted_Access, 2, W_32));
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
            --  Call
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
              + Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_32));
            return;

         when 16#9a# =>
            --  Callf
            Branch     := Br_Call;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_32));
            return;

         when 16#e9# =>
            --  jmp rel32
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              FT_Dest.Target
                + Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_32));
            return;

         when 16#ea# =>
            --  jmp ptr32
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 5;
            Branch_Dest.Target :=
              Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_32));
            return;

         when 16#eb# =>
            --  jmp rel8
            Branch     := Br_Jmp;
            Flag_Cond  := False;
            Flag_Indir := False;
            FT_Dest.Target := Pc + 2;
            Branch_Dest.Target :=
              FT_Dest.Target
              + Pc_Type (Decode_Val (Mem'Unrestricted_Access, 1, W_8));
            return;

         when 16#ff# =>
            B1 := Insn_Bin (Insn_Bin'First + 1);
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
