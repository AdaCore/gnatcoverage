------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2006 Tristan Gingold                    --
--                        Copyright (C) 2009, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package body Disa_X86 is
   subtype Byte is Interfaces.Unsigned_8;
   type Bf_2 is mod 2 ** 2;
   type Bf_3 is mod 2 ** 3;
   type Bf_6 is mod 2 ** 6;

   type Width_Type is (W_None, W_8, W_16, W_32, W_Data);
   subtype String8 is String (1 .. 8);

   type Code_Type is
     (C_None,
      C_Prefix,
      C_Prefix_Rep,

      --  Start of Modrm.
      C_Eb,
      C_Ep,
      C_Ev,
      C_Ev_Iz,
      C_Ev_Ib,
      C_Ew,

      C_Ma,
      C_Mp,
      C_Mfs,
      C_Mfd,
      C_Mfe,
      C_Md,
      C_Mw,
      C_Mq,
      C_M,
      --  End of modrm.

      C_Gw,
      C_Gz,
      C_Gb,
      C_Gv,
      C_Gv_Ib,
      C_Gv_Cl,

      C_Reg_Al,
      C_Reg_Cl,
      C_Reg_Dl,
      C_Reg_Bl,
      C_Reg_Ch,
      C_Reg_Dh,
      C_Reg_Bh,
      C_Reg_Ah,
      C_Reg_Ax,
      C_Reg_Cx,
      C_Reg_Bx,
      C_Reg_Dx,
      C_Reg_Sp,
      C_Reg_Bp,
      C_Reg_Si,
      C_Reg_Di,

      C_Iv,
      C_Ib,
      C_Iz,
      C_Iw,

      C_Yb,
      C_Yz,
      C_Yv,

      C_Xb,
      C_Xz,
      C_Xv,

      C_Jb,
      C_Jz,

      C_Sw,
      C_Ap,
      C_Fv,

      C_Ob,
      C_Ov,

      C_H0, --  st(0)
      C_H,  --  st(X)

      C_Reg_Es,
      C_Reg_Ss,
      C_Reg_Cs,
      C_Reg_Ds,
      C_Cst_1
     );

   subtype Modrm_Code is Code_Type range C_Eb .. C_M;

   --  Description for one instruction.
   type Insn_Desc_Type is record
      --  Name of the operation.
      Name : String8;

      Dst, Src : Code_Type;
   end record;

   type Insn_Desc_Array_Type is array (Byte) of Insn_Desc_Type;
   type Group_Desc_Array_Type is array (Bf_3) of Insn_Desc_Type;
   Insn_Desc : constant Insn_Desc_Array_Type :=
     (
      --  00-07
      2#00_000_000# => ("add     ", C_Eb, C_Gb),
      2#00_000_001# => ("add     ", C_Ev, C_Gv),
      2#00_000_010# => ("add     ", C_Gb, C_Eb),
      2#00_000_011# => ("add     ", C_Gv, C_Ev),
      2#00_000_100# => ("add     ", C_Reg_Al, C_Ib),
      2#00_000_101# => ("add     ", C_Reg_Ax, C_Iz),

      2#00_000_110# => ("push    ", C_Reg_Es, C_None),
      2#00_000_111# => ("pop     ", C_Reg_Es, C_None),

      --  08-0F
      2#00_001_000# => ("or      ", C_Eb, C_Gb),
      2#00_001_001# => ("or      ", C_Ev, C_Gv),
      2#00_001_010# => ("or      ", C_Gb, C_Eb),
      2#00_001_011# => ("or      ", C_Gv, C_Ev),
      2#00_001_100# => ("or      ", C_Reg_Al, C_Ib),
      2#00_001_101# => ("or      ", C_Reg_Ax, C_Iz),

      2#00_001_110# => ("push    ", C_Reg_Cs, C_None),
      2#00_001_111# => ("-       ", C_None, C_None),

      --  10-17
      2#00_010_000# => ("adc     ", C_Eb, C_Gb),
      2#00_010_001# => ("adc     ", C_Ev, C_Gv),
      2#00_010_010# => ("adc     ", C_Gb, C_Eb),
      2#00_010_011# => ("adc     ", C_Gv, C_Ev),
      2#00_010_100# => ("adc     ", C_Reg_Al, C_Ib),
      2#00_010_101# => ("adc     ", C_Reg_Ax, C_Iz),

      2#00_010_110# => ("push    ", C_Reg_Ss, C_None),
      2#00_010_111# => ("pop     ", C_Reg_Ss, C_None),

      --  18-1F
      2#00_011_000# => ("sbb     ", C_Eb, C_Gb),
      2#00_011_001# => ("sbb     ", C_Ev, C_Gv),
      2#00_011_010# => ("sbb     ", C_Gb, C_Eb),
      2#00_011_011# => ("sbb     ", C_Gv, C_Ev),
      2#00_011_100# => ("sbb     ", C_Reg_Al, C_Ib),
      2#00_011_101# => ("sbb     ", C_Reg_Ax, C_Iz),

      2#00_011_110# => ("push    ", C_Reg_Ds, C_None),
      2#00_011_111# => ("pop     ", C_Reg_Ds, C_None),

      --  20-27
      2#00_100_000# => ("and     ", C_Eb, C_Gb),
      2#00_100_001# => ("and     ", C_Ev, C_Gv),
      2#00_100_010# => ("and     ", C_Gb, C_Eb),
      2#00_100_011# => ("and     ", C_Gv, C_Ev),
      2#00_100_100# => ("and     ", C_Reg_Al, C_Ib),
      2#00_100_101# => ("and     ", C_Reg_Ax, C_Iz),

      2#00_100_110# => ("es      ", C_Prefix, C_None),
      2#00_100_111# => ("daa     ", C_None, C_None),

      --  28-2F
      2#00_101_000# => ("sub     ", C_Eb, C_Gb),
      2#00_101_001# => ("sub     ", C_Ev, C_Gv),
      2#00_101_010# => ("sub     ", C_Gb, C_Eb),
      2#00_101_011# => ("sub     ", C_Gv, C_Ev),
      2#00_101_100# => ("sub     ", C_Reg_Al, C_Ib),
      2#00_101_101# => ("sub     ", C_Reg_Ax, C_Iz),

      2#00_101_110# => ("cs      ", C_Prefix, C_None),
      2#00_101_111# => ("das     ", C_None, C_None),

      --  30-37
      2#00_110_000# => ("xor     ", C_Eb, C_Gb),
      2#00_110_001# => ("xor     ", C_Ev, C_Gv),
      2#00_110_010# => ("xor     ", C_Gb, C_Eb),
      2#00_110_011# => ("xor     ", C_Gv, C_Ev),
      2#00_110_100# => ("xor     ", C_Reg_Al, C_Ib),
      2#00_110_101# => ("xor     ", C_Reg_Ax, C_Iz),

      2#00_110_110# => ("ss      ", C_Prefix, C_None),
      2#00_110_111# => ("aaa     ", C_None, C_None),

      --  28-2F
      2#00_111_000# => ("cmp     ", C_Eb, C_Gb),
      2#00_111_001# => ("cmp     ", C_Ev, C_Gv),
      2#00_111_010# => ("cmp     ", C_Gb, C_Eb),
      2#00_111_011# => ("cmp     ", C_Gv, C_Ev),
      2#00_111_100# => ("cmp     ", C_Reg_Al, C_Ib),
      2#00_111_101# => ("cmp     ", C_Reg_Ax, C_Iz),

      2#00_111_110# => ("ds      ", C_Prefix, C_None),
      2#00_111_111# => ("aas     ", C_None, C_None),

      --  40-4F
      16#40#        => ("inc     ", C_Reg_Ax, C_None),
      16#41#        => ("inc     ", C_Reg_Cx, C_None),
      16#42#        => ("inc     ", C_Reg_Dx, C_None),
      16#43#        => ("inc     ", C_Reg_Bx, C_None),
      16#44#        => ("inc     ", C_Reg_Sp, C_None),
      16#45#        => ("inc     ", C_Reg_Bp, C_None),
      16#46#        => ("inc     ", C_Reg_Si, C_None),
      16#47#        => ("inc     ", C_Reg_Di, C_None),

      16#48#        => ("dec     ", C_Reg_Ax, C_None),
      16#49#        => ("dec     ", C_Reg_Cx, C_None),
      16#4a#        => ("dec     ", C_Reg_Dx, C_None),
      16#4b#        => ("dec     ", C_Reg_Bx, C_None),
      16#4c#        => ("dec     ", C_Reg_Sp, C_None),
      16#4d#        => ("dec     ", C_Reg_Bp, C_None),
      16#4e#        => ("dec     ", C_Reg_Si, C_None),
      16#4f#        => ("dec     ", C_Reg_Di, C_None),

      --  50-5F
      16#50#        => ("push    ", C_Reg_Ax, C_None),
      16#51#        => ("push    ", C_Reg_Cx, C_None),
      16#52#        => ("push    ", C_Reg_Dx, C_None),
      16#53#        => ("push    ", C_Reg_Bx, C_None),
      16#54#        => ("push    ", C_Reg_Sp, C_None),
      16#55#        => ("push    ", C_Reg_Bp, C_None),
      16#56#        => ("push    ", C_Reg_Si, C_None),
      16#57#        => ("push    ", C_Reg_Di, C_None),

      16#58#        => ("pop     ", C_Reg_Ax, C_None),
      16#59#        => ("pop     ", C_Reg_Cx, C_None),
      16#5a#        => ("pop     ", C_Reg_Dx, C_None),
      16#5b#        => ("pop     ", C_Reg_Bx, C_None),
      16#5c#        => ("pop     ", C_Reg_Sp, C_None),
      16#5d#        => ("pop     ", C_Reg_Bp, C_None),
      16#5e#        => ("pop     ", C_Reg_Si, C_None),
      16#5f#        => ("pop     ", C_Reg_Di, C_None),

      --  60-6F
      16#60#        => ("pusha   ", C_None, C_None),
      16#61#        => ("popa    ", C_None, C_None),
      16#62#        => ("bound   ", C_Gv, C_Ma),
      16#63#        => ("arpl    ", C_Ew, C_Gw),
      16#64#        => ("fs      ", C_Prefix, C_None),
      16#65#        => ("gs      ", C_Prefix, C_None),
      16#66#        => ("oper    ", C_Prefix, C_None),
      16#67#        => ("addr    ", C_Prefix, C_None),

      16#68#        => ("push    ", C_Iz, C_None),
      16#69#        => ("imul    ", C_Gv, C_Ev_Iz),
      16#6a#        => ("push    ", C_Ib, C_None),
      16#6b#        => ("imul    ", C_Gv, C_Ev_Ib),
      16#6c#        => ("ins     ", C_Yb, C_Reg_Dx),
      16#6d#        => ("ins     ", C_Yz, C_Reg_Dx),
      16#6e#        => ("outs    ", C_Reg_Dx, C_Xb),
      16#6f#        => ("outs    ", C_Reg_Dx, C_Xz),

      --  70-7F
      2#0111_0000#  => ("jo      ", C_Jb, C_None),
      2#0111_0001#  => ("jno     ", C_Jb, C_None),
      2#0111_0010#  => ("jb      ", C_Jb, C_None),
      2#0111_0011#  => ("jae     ", C_Jb, C_None),
      2#0111_0100#  => ("je      ", C_Jb, C_None),
      2#0111_0101#  => ("jne     ", C_Jb, C_None),
      2#0111_0110#  => ("jbe     ", C_Jb, C_None),
      2#0111_0111#  => ("ja      ", C_Jb, C_None),
      2#0111_1000#  => ("js      ", C_Jb, C_None),
      2#0111_1001#  => ("jns     ", C_Jb, C_None),
      2#0111_1010#  => ("jp      ", C_Jb, C_None),
      2#0111_1011#  => ("jnp     ", C_Jb, C_None),
      2#0111_1100#  => ("jl      ", C_Jb, C_None),
      2#0111_1101#  => ("jge     ", C_Jb, C_None),
      2#0111_1110#  => ("jle     ", C_Jb, C_None),
      2#0111_1111#  => ("jg      ", C_Jb, C_None),

      --  80-8F
      2#1000_0000#  => ("1       ", C_Eb, C_Ib),
      2#1000_0001#  => ("1       ", C_Ev, C_Iz),
      2#1000_0010#  => ("1       ", C_Eb, C_Ib),
      2#1000_0011#  => ("1       ", C_Ev, C_Ib),

      2#1000_0100#  => ("test    ", C_Eb, C_Gb),
      2#1000_0101#  => ("test    ", C_Ev, C_Gv),
      2#1000_0110#  => ("xchg    ", C_Eb, C_Gb),
      2#1000_0111#  => ("xchg    ", C_Eb, C_Gb),

      2#1000_1000#  => ("mov     ", C_Eb, C_Gb),
      2#1000_1001#  => ("mov     ", C_Ev, C_Gv),
      2#1000_1010#  => ("mov     ", C_Gb, C_Eb),
      2#1000_1011#  => ("mov     ", C_Gv, C_Ev),
      2#1000_1100#  => ("mov     ", C_Ev, C_Sw),
      2#1000_1101#  => ("lea     ", C_Gv, C_M),
      2#1000_1110#  => ("mov     ", C_Sw, C_Ew),
      2#1000_1111#  => ("pop     ", C_Ev, C_None),

      --  90-9F
      2#1001_0000#  => ("nop     ", C_None, C_None),
      16#91#        => ("xchg    ", C_Reg_Ax, C_Reg_Cx),
      16#92#        => ("xchg    ", C_Reg_Ax, C_Reg_Dx),
      16#93#        => ("xchg    ", C_Reg_Ax, C_Reg_Bx),
      16#94#        => ("xchg    ", C_Reg_Ax, C_Reg_Sp),
      16#95#        => ("xchg    ", C_Reg_Ax, C_Reg_Bp),
      16#96#        => ("xchg    ", C_Reg_Ax, C_Reg_Si),
      16#97#        => ("xchg    ", C_Reg_Ax, C_Reg_Di),

      16#98#        => ("cbw     ", C_None, C_None),
      16#99#        => ("cwd     ", C_None, C_None),
      16#9a#        => ("callf   ", C_Ap, C_None),
      16#9b#        => ("fwait   ", C_None, C_None),
      16#9c#        => ("pushf   ", C_Fv, C_None),
      16#9d#        => ("popf    ", C_Fv, C_None),
      16#9e#        => ("sahf    ", C_None, C_None),
      16#9f#        => ("lahf    ", C_None, C_None),

      --  A0-AF
      16#A0#        => ("mov     ", C_Reg_Al, C_Ob),
      16#A1#        => ("mov     ", C_Reg_Ax, C_Ov),
      16#A2#        => ("mov     ", C_Ob, C_Reg_Al),
      16#A3#        => ("mov     ", C_Ov, C_Reg_Ax),

      16#A4#        => ("movs    ", C_Xb, C_Yb),
      16#A5#        => ("movs    ", C_Xv, C_Yv),
      16#A6#        => ("cmps    ", C_Xb, C_Yb),
      16#A7#        => ("cmps    ", C_Xv, C_Yv),

      16#A8#        => ("test    ", C_Reg_Al, C_Ib),
      16#A9#        => ("test    ", C_Reg_Ax, C_Iz),
      16#Aa#        => ("stos    ", C_Yb, C_Reg_Al),
      16#Ab#        => ("stos    ", C_Yv, C_Reg_Ax),
      16#Ac#        => ("lods    ", C_Reg_Al, C_Xb),
      16#Ad#        => ("lods    ", C_Reg_Ax, C_Xv),
      16#Ae#        => ("scas    ", C_Reg_Al, C_Xb), -- FIXME: Xb or Yb?
      16#Af#        => ("scas    ", C_Reg_Ax, C_Xv),

      --  B0-BF
      16#B0#        => ("mov     ", C_Reg_Al, C_Ib),
      16#B1#        => ("mov     ", C_Reg_Cl, C_Ib),
      16#B2#        => ("mov     ", C_Reg_Dl, C_Ib),
      16#B3#        => ("mov     ", C_Reg_Bl, C_Ib),
      16#B4#        => ("mov     ", C_Reg_Ah, C_Ib),
      16#B5#        => ("mov     ", C_Reg_Ch, C_Ib),
      16#B6#        => ("mov     ", C_Reg_Dh, C_Ib),
      16#B7#        => ("mov     ", C_Reg_Bh, C_Ib),
      16#B8#        => ("mov     ", C_Reg_Ax, C_Iv),
      16#B9#        => ("mov     ", C_Reg_Cx, C_Iv),
      16#Ba#        => ("mov     ", C_Reg_Dx, C_Iv),
      16#Bb#        => ("mov     ", C_Reg_Bx, C_Iv),
      16#Bc#        => ("mov     ", C_Reg_Sp, C_Iv),
      16#Bd#        => ("mov     ", C_Reg_Bp, C_Iv),
      16#Be#        => ("mov     ", C_Reg_Si, C_Iv),
      16#Bf#        => ("mov     ", C_Reg_Di, C_Iv),

      --  C0-CF
      16#C0#        => ("2       ", C_Eb, C_Ib),
      16#C1#        => ("2       ", C_Ev, C_Ib),

      16#C2#        => ("ret     ", C_Iw, C_None),
      16#C3#        => ("ret     ", C_None, C_None),
      16#C4#        => ("les     ", C_Gz, C_Mp),
      16#C5#        => ("lds     ", C_Gz, C_Mp),
      16#C6#        => ("mov     ", C_Eb, C_Ib),
      16#C7#        => ("mov     ", C_Ev, C_Iz),

      16#C8#        => ("enter   ", C_Iw, C_Ib),
      16#C9#        => ("leave   ", C_None, C_None),
      16#Ca#        => ("retf    ", C_Iw, C_None),
      16#Cb#        => ("retf    ", C_None, C_None),
      16#Cc#        => ("int3    ", C_None, C_None),
      16#Cd#        => ("int     ", C_Ib, C_None),
      16#Ce#        => ("into    ", C_None, C_None),
      16#Cf#        => ("iret    ", C_None, C_None),

      --  D0-DF
      16#D0#        => ("2       ", C_Eb, C_Cst_1),
      16#D1#        => ("2       ", C_Ev, C_Cst_1),
      16#D2#        => ("2       ", C_Eb, C_Reg_Cl),
      16#D3#        => ("2       ", C_Ev, C_Reg_Cl),
      16#D4#        => ("aam     ", C_Ib, C_None),
      16#D5#        => ("aad     ", C_Ib, C_None),
      16#D6#        => ("        ", C_None, C_None),
      16#D7#        => ("xlat    ", C_None, C_None),
      16#D8#        => ("ESC     ", C_M, C_None),
      16#D9#        => ("ESC     ", C_M, C_None),
      16#Da#        => ("ESC     ", C_M, C_None),
      16#Db#        => ("ESC     ", C_M, C_None),
      16#Dc#        => ("ESC     ", C_M, C_None),
      16#Dd#        => ("ESC     ", C_M, C_None),
      16#De#        => ("ESC     ", C_M, C_None),
      16#Df#        => ("ESC     ", C_M, C_None),

      --  E0-EF
      16#E0#        => ("loopne  ", C_Jb, C_None),
      16#E1#        => ("loope   ", C_Jb, C_None),
      16#E2#        => ("loop    ", C_Jb, C_None),
      16#E3#        => ("jrcxz   ", C_Jb, C_None),
      16#E4#        => ("in      ", C_Reg_Al, C_Ib),
      16#E5#        => ("in      ", C_Reg_Ax, C_Ib),
      16#E6#        => ("out     ", C_Ib, C_Reg_Al),
      16#E7#        => ("out     ", C_Ib, C_Reg_Ax),

      16#E8#        => ("call    ", C_Jz, C_None),
      16#E9#        => ("jmp     ", C_Jz, C_None),
      16#Ea#        => ("jmpf    ", C_Ap, C_None),
      16#Eb#        => ("jmp     ", C_Jb, C_None),
      16#Ec#        => ("in      ", C_Reg_Al, C_Reg_Dx),
      16#Ed#        => ("in      ", C_Reg_Ax, C_Reg_Dx),
      16#Ee#        => ("out     ", C_Reg_Dx, C_Reg_Al),
      16#Ef#        => ("out     ", C_Reg_Dx, C_Reg_Ax),

      --  F0-FF
      16#F0#        => ("lock    ", C_Prefix, C_None),
      16#F1#        => ("        ", C_None, C_None),
      16#F2#        => ("repne   ", C_Prefix_Rep, C_None),
      16#F3#        => ("rep     ", C_Prefix_Rep, C_None),
      16#F4#        => ("hlt     ", C_None, C_None),
      16#F5#        => ("cmc     ", C_None, C_None),
      16#F6#        => ("3       ", C_Eb, C_None),
      16#F7#        => ("3       ", C_Ev, C_None),
      16#F8#        => ("clc     ", C_None, C_None),
      16#F9#        => ("stc     ", C_None, C_None),
      16#Fa#        => ("cli     ", C_None, C_None),
      16#Fb#        => ("sti     ", C_None, C_None),
      16#Fc#        => ("cld     ", C_None, C_None),
      16#Fd#        => ("std     ", C_None, C_None),
      16#Fe#        => ("4       ", C_None, C_None),
      16#Ff#        => ("5       ", C_None, C_None));

   Insn_Desc_0F : constant Insn_Desc_Array_Type :=
     (
      16#00#       => ("6       ", C_None, C_None),
      16#01#       => ("7       ", C_None, C_None),
      16#02#       => ("lar     ", C_Gv, C_Ew),
      16#03#       => ("lsl     ", C_Gv, C_Ew),
      16#04#       => ("        ", C_None, C_None),
      16#05#       => ("syscall ", C_None, C_None),
      16#06#       => ("clts    ", C_None, C_None),
      16#07#       => ("sysret  ", C_None, C_None),
      16#08#       => ("invd    ", C_None, C_None),
      16#09#       => ("wbinvd  ", C_None, C_None),
      16#0a#       => ("        ", C_None, C_None),
      16#0b#       => ("ud2     ", C_None, C_None),
      16#0c#       => ("        ", C_None, C_None),
      16#0d#       => ("nop     ", C_Ev, C_None),
      16#0e#       => ("        ", C_None, C_None),
      16#0f#       => ("        ", C_None, C_None),

      2#1000_0000# => ("jo      ", C_Jz, C_None),
      2#1000_0001# => ("jno     ", C_Jz, C_None),
      2#1000_0010# => ("jb      ", C_Jz, C_None),
      2#1000_0011# => ("jae     ", C_Jz, C_None),
      2#1000_0100# => ("je      ", C_Jz, C_None),
      2#1000_0101# => ("jne     ", C_Jz, C_None),
      2#1000_0110# => ("jbe     ", C_Jz, C_None),
      2#1000_0111# => ("ja      ", C_Jz, C_None),
      2#1000_1000# => ("js      ", C_Jz, C_None),
      2#1000_1001# => ("jns     ", C_Jz, C_None),
      2#1000_1010# => ("jp      ", C_Jz, C_None),
      2#1000_1011# => ("jnp     ", C_Jz, C_None),
      2#1000_1100# => ("jl      ", C_Jz, C_None),
      2#1000_1101# => ("jge     ", C_Jz, C_None),
      2#1000_1110# => ("jle     ", C_Jz, C_None),
      2#1000_1111# => ("jg      ", C_Jz, C_None),

      2#1001_0000# => ("seto    ", C_Eb, C_None),
      2#1001_0001# => ("setno   ", C_Eb, C_None),
      2#1001_0010# => ("setb    ", C_Eb, C_None),
      2#1001_0011# => ("setae   ", C_Eb, C_None),
      2#1001_0100# => ("sete    ", C_Eb, C_None),
      2#1001_0101# => ("setne   ", C_Eb, C_None),
      2#1001_0110# => ("setbe   ", C_Eb, C_None),
      2#1001_0111# => ("seta    ", C_Eb, C_None),
      2#1001_1000# => ("sets    ", C_Eb, C_None),
      2#1001_1001# => ("setns   ", C_Eb, C_None),
      2#1001_1010# => ("setp    ", C_Eb, C_None),
      2#1001_1011# => ("setnp   ", C_Eb, C_None),
      2#1001_1100# => ("setl    ", C_Eb, C_None),
      2#1001_1101# => ("setge   ", C_Eb, C_None),
      2#1001_1110# => ("setle   ", C_Eb, C_None),
      2#1001_1111# => ("setjg   ", C_Eb, C_None),

      16#A4#       => ("shld    ", C_Ev, C_Gv_Ib),
      16#A5#       => ("shld    ", C_Ev, C_Gv_Cl),
      16#Ac#       => ("shrd    ", C_Ev, C_Gv_Ib),
      16#Af#       => ("imul    ", C_Gv, C_Ev),

      16#B6#       => ("movzx   ", C_Gv, C_Eb),
      16#B7#       => ("movzx   ", C_Gv, C_Ew),
      16#BB#       => ("btc     ", C_Ev, C_Gv),
      16#BC#       => ("bsf     ", C_Gv, C_Ev),
      16#BD#       => ("bsr     ", C_Gv, C_Ev),
      16#BE#       => ("movsx   ", C_Gv, C_Eb),
      16#BF#       => ("movsx   ", C_Gv, C_Ew),
      others       => ("        ", C_None, C_None));

   subtype String3 is String (1 .. 3);
   type Group_Name_Array_Type is array (Bf_3) of String3;
   Group_Name_1 : constant Group_Name_Array_Type :=
     ("add", "or ", "adc", "sbb", "and", "sub", "xor", "cmp");
   Group_Name_2 : constant Group_Name_Array_Type :=
     ("rol", "ror", "rcl", "rcr", "shl", "shr", "   ", "sar");

   --  16#F7#
   Insn_Desc_G3 : constant Group_Desc_Array_Type :=
     (2#000# => ("test    ", C_Ib, C_Iz),
      2#010# => ("not     ", C_None, C_None),
      2#011# => ("neg     ", C_None, C_None),
      2#100# => ("mul     ", C_Reg_Al, C_Reg_Ax),
      2#101# => ("imul    ", C_Reg_Al, C_Reg_Ax),
      2#110# => ("div     ", C_Reg_Al, C_Reg_Ax),
      2#111# => ("idiv    ", C_Reg_Al, C_Reg_Ax),
      others => ("        ", C_None, C_None));

   Insn_Desc_G5 : constant Group_Desc_Array_Type :=
     (2#000# => ("inc     ", C_Ev, C_None),
      2#001# => ("dec     ", C_Ev, C_None),
      2#010# => ("call    ", C_Ev, C_None),
      2#011# => ("callf   ", C_Ep, C_None),
      2#100# => ("jmp     ", C_Ev, C_None),
      2#101# => ("jmpf    ", C_Ep, C_None),
      2#110# => ("push    ", C_Ev, C_None),
      2#111# => ("        ", C_None, C_None));

   type Esc_Desc_Array_Type is array (Bf_3, Bf_3) of Insn_Desc_Type;
   Insn_Desc_Esc : constant Esc_Desc_Array_Type :=
     (
      --  D8
      (2#000# => ("fadd    ", C_Mfs, C_None),
       2#001# => ("fmul    ", C_Mfs, C_None),
       2#010# => ("fcom    ", C_Mfs, C_None),
       2#011# => ("fcomp   ", C_Mfs, C_None),
       2#100# => ("fsub    ", C_Mfs, C_None),
       2#101# => ("fsubr   ", C_Mfs, C_None),
       2#110# => ("fdiv    ", C_Mfs, C_None),
       2#111# => ("fdivr   ", C_Mfs, C_None)),
      --  D9
      (2#000# => ("fld     ", C_Mfs, C_None),
       2#001# => ("        ", C_None, C_None),
       2#010# => ("fst     ", C_Mfs, C_None),
       2#011# => ("fstp    ", C_Mfs, C_None),
       2#100# => ("fldenv  ", C_M, C_None),
       2#101# => ("fldcw   ", C_Mfs, C_None),
       2#110# => ("fstenv  ", C_Mfs, C_None),
       2#111# => ("fstcw   ", C_Mfs, C_None)),
      --  DA
      (2#000# => ("fiadd   ", C_Md, C_None),
       2#001# => ("fimul   ", C_Md, C_None),
       2#010# => ("ficom   ", C_Md, C_None),
       2#011# => ("ficomp  ", C_Md, C_None),
       2#100# => ("fisub   ", C_Md, C_None),
       2#101# => ("fisubr  ", C_Md, C_None),
       2#110# => ("fidiv   ", C_Md, C_None),
       2#111# => ("fidivr  ", C_Md, C_None)),
      --  DB
      (2#000# => ("fild    ", C_Md, C_None),
       2#001# => ("fisttp  ", C_Md, C_None),
       2#010# => ("fist    ", C_Md, C_None),
       2#011# => ("fistp   ", C_Md, C_None),
       2#100# => ("        ", C_None, C_None),
       2#101# => ("fld     ", C_Mfe, C_None),
       2#110# => ("        ", C_None, C_None),
       2#111# => ("fstp    ", C_Mfe, C_None)),
      --  DC
      (2#000# => ("fadd    ", C_Mfd, C_None),
       2#001# => ("fmul    ", C_Mfd, C_None),
       2#010# => ("fcom    ", C_Mfd, C_None),
       2#011# => ("fcomp   ", C_Mfd, C_None),
       2#100# => ("fsub    ", C_Mfd, C_None),
       2#101# => ("fsubr   ", C_Mfd, C_None),
       2#110# => ("fdiv    ", C_Mfd, C_None),
       2#111# => ("fdivr   ", C_Mfd, C_None)),
      --  DD
      (2#000# => ("fld     ", C_Mfd, C_None),
       2#001# => ("fisttp  ", C_Mq, C_None),
       2#010# => ("fst     ", C_Mfd, C_None),
       2#011# => ("fstp    ", C_Mfd, C_None),
       2#100# => ("frstor  ", C_M, C_None),
       2#101# => ("        ", C_None, C_None),
       2#110# => ("fsave   ", C_M, C_None),
       2#111# => ("fstsw   ", C_M, C_None)),
      --  DE
      (2#000# => ("fiadd   ", C_Mw, C_None),
       2#001# => ("fimul   ", C_Mw, C_None),
       2#010# => ("ficom   ", C_Mw, C_None),
       2#011# => ("ficomp  ", C_Mw, C_None),
       2#100# => ("fisub   ", C_Mw, C_None),
       2#101# => ("fisubr  ", C_Mw, C_None),
       2#110# => ("fidiv   ", C_Mw, C_None),
       2#111# => ("fidivr  ", C_Mw, C_None)),
      --  DF
      (2#000# => ("fild    ", C_Md, C_None),
       2#001# => ("fisttp  ", C_Md, C_None),
       2#010# => ("fist    ", C_Md, C_None),
       2#011# => ("fistp   ", C_Md, C_None),
       2#100# => ("fbld    ", C_M, C_None),
       2#101# => ("fild    ", C_Mq, C_None),
       2#110# => ("fbstp   ", C_M, C_None),
       2#111# => ("fistp   ", C_Mq, C_None)));

   type Sub_Esc_Desc_Array_Type is array (Bf_6) of Insn_Desc_Type;
   Insn_Desc_Esc_D9 : constant Sub_Esc_Desc_Array_Type :=
     (16#00# => ("fld     ", C_H0, C_H),
      16#01# => ("fld     ", C_H0, C_H),
      16#02# => ("fld     ", C_H0, C_H),
      16#03# => ("fld     ", C_H0, C_H),
      16#04# => ("fld     ", C_H0, C_H),
      16#05# => ("fld     ", C_H0, C_H),
      16#06# => ("fld     ", C_H0, C_H),
      16#07# => ("fld     ", C_H0, C_H),
      16#08# => ("fxch    ", C_H0, C_H),
      16#09# => ("fxch    ", C_H0, C_H),
      16#0a# => ("fxch    ", C_H0, C_H),
      16#0b# => ("fxch    ", C_H0, C_H),
      16#0c# => ("fxch    ", C_H0, C_H),
      16#0d# => ("fxch    ", C_H0, C_H),
      16#0e# => ("fxch    ", C_H0, C_H),
      16#0f# => ("fxch    ", C_H0, C_H),

      16#10# => ("fnop    ", C_None, C_None),
      16#11# => ("        ", C_None, C_None),
      16#12# => ("        ", C_None, C_None),
      16#13# => ("        ", C_None, C_None),
      16#14# => ("        ", C_None, C_None),
      16#15# => ("        ", C_None, C_None),
      16#16# => ("        ", C_None, C_None),
      16#17# => ("        ", C_None, C_None),
      16#18# => ("        ", C_None, C_None),
      16#19# => ("        ", C_None, C_None),
      16#1a# => ("        ", C_None, C_None),
      16#1b# => ("        ", C_None, C_None),
      16#1c# => ("        ", C_None, C_None),
      16#1d# => ("        ", C_None, C_None),
      16#1e# => ("        ", C_None, C_None),
      16#1f# => ("        ", C_None, C_None),

      16#20# => ("fchs    ", C_None, C_None),
      16#21# => ("fabs    ", C_None, C_None),
      16#22# => ("        ", C_None, C_None),
      16#23# => ("        ", C_None, C_None),
      16#24# => ("ftst    ", C_None, C_None),
      16#25# => ("fxam    ", C_None, C_None),
      16#26# => ("        ", C_None, C_None),
      16#27# => ("        ", C_None, C_None),
      16#28# => ("fld1    ", C_None, C_None),
      16#29# => ("fldl2t  ", C_None, C_None),
      16#2a# => ("fldl2e  ", C_None, C_None),
      16#2b# => ("fldpi   ", C_None, C_None),
      16#2c# => ("fldlg2  ", C_None, C_None),
      16#2d# => ("fldln2  ", C_None, C_None),
      16#2e# => ("fldlz   ", C_None, C_None),
      16#2f# => ("        ", C_None, C_None),

      16#30# => ("f2xm1   ", C_None, C_None),
      16#31# => ("fyl2x   ", C_None, C_None),
      16#32# => ("fptan   ", C_None, C_None),
      16#33# => ("fpatan  ", C_None, C_None),
      16#34# => ("fpxtract", C_None, C_None),
      16#35# => ("fprem1  ", C_None, C_None),
      16#36# => ("fdecstp ", C_None, C_None),
      16#37# => ("fincstp ", C_None, C_None),
      16#38# => ("fprem   ", C_None, C_None),
      16#39# => ("fyl2xp1 ", C_None, C_None),
      16#3a# => ("fsqrt   ", C_None, C_None),
      16#3b# => ("fsincos ", C_None, C_None),
      16#3c# => ("frndint ", C_None, C_None),
      16#3d# => ("fscale  ", C_None, C_None),
      16#3e# => ("fsin    ", C_None, C_None),
      16#3f# => ("fcos    ", C_None, C_None));

   Insn_Desc_Esc_DA : constant Sub_Esc_Desc_Array_Type :=
     (
      16#00# .. 16#07# => ("fcmovb  ", C_H0, C_H),
      16#08# .. 16#0f# => ("fcmove  ", C_H0, C_H),
      16#10# .. 16#17# => ("fcmovbe ", C_H0, C_H),
      16#18# .. 16#1f# => ("fcmovu  ", C_H0, C_H),
      16#29#           => ("fucompp ", C_None, C_None),
      others           => ("        ", C_None, C_None)
     );

   Insn_Desc_Esc_DE : constant Sub_Esc_Desc_Array_Type :=
     (
      16#00# .. 16#07# => ("faddp   ", C_H0, C_H),
      16#08# .. 16#0f# => ("fmulp   ", C_H0, C_H),
      16#19#           => ("fcomp   ", C_None, C_None),
      16#20# .. 16#27# => ("fsubrp  ", C_H0, C_H),
      16#28# .. 16#2f# => ("fsubp   ", C_H0, C_H),
      16#30# .. 16#37# => ("fdivrp  ", C_H0, C_H),
      16#38# .. 16#3f# => ("fdivp   ", C_H0, C_H),
      others           => ("        ", C_None, C_None)
     );

   Insn_Desc_Esc_DF : constant Sub_Esc_Desc_Array_Type :=
     (
      16#20#           => ("fstsw   ", C_Reg_Ax, C_None),
      16#28# .. 16#2f# => ("fucomip ", C_H0, C_H),
      16#30# .. 16#37# => ("fcompip ", C_H0, C_H),
      others           => ("        ", C_None, C_None)
     );

   type Esc_Desc3_Array_Type is array (Bf_3) of Insn_Desc_Type;
   Insn_Desc_Esc_D8 : constant Esc_Desc3_Array_Type :=
     (
      0 => ("fadd    ", C_H0, C_H),
      1 => ("fmul    ", C_H0, C_H),
      2 => ("fcom    ", C_H0, C_H),
      3 => ("fcomp   ", C_H0, C_H),
      4 => ("fsub    ", C_H0, C_H),
      5 => ("fsubr   ", C_H0, C_H),
      6 => ("fdiv    ", C_H0, C_H),
      7 => ("fdivr   ", C_H0, C_H)
     );

   Insn_Desc_Esc_DD : constant Esc_Desc3_Array_Type :=
     (
      0 => ("ffree   ", C_H, C_None),
      1 => ("        ", C_None, C_None),
      2 => ("fst     ", C_H, C_None),
      3 => ("fstp    ", C_H, C_None),
      4 => ("fucom   ", C_H, C_H0),
      5 => ("fucomp  ", C_H, C_None),
      6 => ("        ", C_None, C_None),
      7 => ("        ", C_None, C_None)
     );

   --  Standard widths of operations.
   type Width_Array_Type is array (Width_Type) of Character;
   Width_Char : constant Width_Array_Type :=
     (W_None => '-', W_8 => 'b', W_16 => 'w', W_32 => 'l', W_Data => '?');
   type Width_Len_Type is array (Width_Type) of Pc_Type;
   Width_Len : constant Width_Len_Type :=
     (W_None => 0, W_8 => 1, W_16 => 2, W_32 => 4, W_Data => 0);

   --  Registers.
--    type Reg_Type is (Reg_Ax, Reg_Bx, Reg_Cx, Reg_Dx,
--                      Reg_Bp, Reg_Sp, Reg_Si, Reg_Di,
--                      Reg_Al, Reg_Ah, Reg_Bl, Reg_Bh,
--                      Reg_Cl, Reg_Ch, Reg_Dl, Reg_Dh);

   --  Bits extraction from byte functions.
   --  For a byte, MSB (most significant bit) is bit 7 while
   --  LSB (least significant bit) is bit 0.

   --  Extract bits 2, 1 and 0.
   function Ext_210 (B : Byte) return Bf_3;
   pragma Inline (Ext_210);

   --  Extract bits 5-3 of byte B.
   function Ext_543 (B : Byte) return Bf_3;
   pragma Inline (Ext_543);

   --  Extract bits 7-6 of byte B.
   function Ext_76 (B : Byte) return Bf_2;
   pragma Inline (Ext_76);

   function Ext_210 (B : Byte) return Bf_3 is
   begin
      return Bf_3 (B and 2#111#);
   end Ext_210;

   function Ext_543 (B : Byte) return Bf_3 is
   begin
      return Bf_3 (Shift_Right (B, 3) and 2#111#);
   end Ext_543;

   function Ext_76 (B : Byte) return Bf_2 is
   begin
      return Bf_2 (Shift_Right (B, 6) and 2#11#);
   end Ext_76;

   function Ext_Modrm_Mod (B : Byte) return Bf_2 renames Ext_76;
   function Ext_Modrm_Rm (B : Byte) return Bf_3 renames Ext_210;
   function Ext_Modrm_Reg (B : Byte) return Bf_3 renames Ext_543;
   function Ext_Sib_Base (B : Byte) return Bf_3 renames Ext_210;
   function Ext_Sib_Index (B : Byte) return Bf_3 renames Ext_543;
   function Ext_Sib_Scale (B : Byte) return Bf_2 renames Ext_76;

   type Hex_Str is array (Natural range 0 .. 15) of Character;
   Hex_Digit : constant Hex_Str := "0123456789abcdef";

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

      --  Index in LINE of the next character to be written.
      Lo : Natural;

      --  The instruction memory, 0 based.
      Mem : Binary_Content renames Insn_Bin;

      --  Add NAME to the line.
      procedure Add_Name (Name : String8);
      pragma Inline (Add_Name);

      --  Add CHAR to the line.
      procedure Add_Char (C : Character);
      pragma Inline (Add_Char);

      --  Add STR to the line.
      procedure Add_String (Str : String);

      --  Add BYTE to the line.
      procedure Add_Byte (V : Byte);

      procedure Add_Comma;
      procedure Name_Align (Orig : Natural);
      procedure Decode_Reg_Field (F : Bf_3; W : Width_Type);
      procedure Decode_Reg_St (F : Bf_3);
      procedure Decode_Val (Off : Pc_Type; Width : Width_Type);
      function Decode_Val (Off : Pc_Type; Width : Width_Type)
                          return Unsigned_32;
      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type);
      procedure Decode_Disp (Off : Pc_Type;
                             Width : Width_Type;
                             Offset : Unsigned_32 := 0);
      procedure Decode_Disp_Rel (Off : in out Pc_Type;
                                 Width : Width_Type);
      procedure Decode_Modrm_Reg (B : Byte; Width : Width_Type);
      procedure Decode_Sib (Sib : Byte; B_Mod : Bf_2);
      procedure Decode_Modrm_Mem (Off : Pc_Type; Width : Width_Type);
      function Decode_Modrm_Len (Off : Pc_Type) return Pc_Type;
      procedure Add_Operand (C : Code_Type;
                             Off_Modrm : Pc_Type;
                             Off_Imm : in out Pc_Type;
                             W : Width_Type);
      procedure Add_Opcode (Name : String8; Width : Width_Type);

      pragma Unreferenced (Add_Opcode);
      --  XXX

      procedure Add_Char (C : Character) is
      begin
         if Lo <= Line'Last then
            Line (Lo) := C;
            Lo := Lo + 1;
         end if;
      end Add_Char;

      --  Add STR to the line.
      procedure Add_String (Str : String) is
      begin
         Line (Lo .. Lo + Str'Length - 1) := Str;
         Lo := Lo + Str'Length;
      end Add_String;

      --  Add BYTE to the line.
      procedure Add_Byte (V : Byte) is
      begin
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 4) and 16#0f#)));
         Add_Char (Hex_Digit (Natural (Shift_Right (V, 0) and 16#0f#)));
      end Add_Byte;

      procedure Add_Name (Name : String8) is
      begin
         for I in Name'Range loop
            exit when Name (I) = ' ';
            Add_Char (Name (I));
         end loop;
      end Add_Name;

      procedure Add_Comma is
      begin
         Add_String (", ");
      end Add_Comma;

      procedure Name_Align (Orig : Natural) is
      begin
         Add_Char (' ');
         while Lo - Orig < 8 loop
            Add_Char (' ');
         end loop;
      end Name_Align;

      procedure Add_Opcode (Name : String8; Width : Width_Type)
      is
         L : constant Natural := Lo;
      begin
         Add_Name (Name);
         if False and Width /= W_None then
            Add_Char (Width_Char (Width));
         end if;
         Name_Align (L);
      end Add_Opcode;

      procedure Decode_Reg_St (F : Bf_3) is
      begin
         Add_String ("%st(");
         Add_Char (Hex_Digit (Natural (F)));
         Add_Char (')');
      end Decode_Reg_St;

      procedure Decode_Reg_Field (F : Bf_3; W : Width_Type) is
         type Reg_Name2_Array is array (Bf_3) of String (1 .. 2);
         type Reg_Name3_Array is array (Bf_3) of String (1 .. 3);
         Regs_8 : constant Reg_Name2_Array :=
           ("al", "cl", "dl", "bl", "ah", "ch", "dh", "bh");
         Regs_16 : constant Reg_Name2_Array :=
           ("ax", "cx", "dx", "bx", "sp", "bp", "si", "di");
         Regs_32 : constant Reg_Name3_Array :=
           ("eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi");
      begin
         Add_Char ('%');
         case W is
            when W_8 =>
               Add_String (Regs_8 (F));
            when W_16 =>
               Add_String (Regs_16 (F));
            when W_32 =>
               Add_String (Regs_32 (F));
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Reg_Field;

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
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Val;

      function Decode_Val (Off : Pc_Type; Width : Width_Type)
                          return Unsigned_32
      is
         V : Unsigned_32;
      begin
         case Width is
            when W_8 =>
               V := Unsigned_32 (Mem (Off));
               --  Sign extension.
               if V >= 16#80# then
                  V := 16#Ffff_Ff00# or V;
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
            when W_None
              | W_Data =>
               raise Program_Error;
         end case;
      end Decode_Val;

      procedure Decode_Imm (Off : in out Pc_Type; Width : Width_Type)
      is
      begin
         Add_String ("$0x");
         Decode_Val (Off, Width);
         Off := Off + Width_Len (Width);
      end Decode_Imm;

      procedure Decode_Disp (Off : Pc_Type;
                             Width : Width_Type;
                             Offset : Unsigned_32 := 0)
      is
         L : Natural;
         V : Unsigned_32;
         Off_Orig : constant Pc_Type := Off;
      begin
         L := Lo;
         V := Decode_Val (Off, Width) + Offset;
         Sym.Symbolize (V, Line, Lo);
         if L /= Lo then
            if V = 0 then
               return;
            end if;
            Add_String (" + ");
         end if;
         Add_String ("0x");
         if Offset = 0 then
            Decode_Val (Off_Orig, Width);
         else
            Add_Byte (Byte (Shift_Right (V, 24) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 16) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 8) and 16#Ff#));
            Add_Byte (Byte (Shift_Right (V, 0) and 16#Ff#));
         end if;
      end Decode_Disp;

      procedure Decode_Disp_Rel (Off : in out Pc_Type;
                                 Width : Width_Type) is
         Disp_Off : constant Pc_Type := Off;
      begin
         Off := Off + Width_Len (Width);
         Decode_Disp (Disp_Off, Width, Off);
      end Decode_Disp_Rel;

      procedure Decode_Modrm_Reg (B : Byte; Width : Width_Type) is
      begin
         Decode_Reg_Field (Ext_Modrm_Reg (B), Width);
      end Decode_Modrm_Reg;

      procedure Decode_Sib (Sib : Byte; B_Mod : Bf_2)
      is
         S : Bf_2;
         I : Bf_3;
         B : Bf_3;
      begin
         S := Ext_Sib_Scale (Sib);
         B := Ext_Sib_Base (Sib);
         I := Ext_Sib_Index (Sib);
         Add_Char ('(');
         if not (B = 2#101# and then B_Mod = 0) then
            --  Base
            Decode_Reg_Field (B, W_32);
            if I /= 2#100# then
               Add_Char (',');
            end if;
         end if;
         if I /= 2#100# then
            --  Index
            Decode_Reg_Field (I, W_32);
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

      procedure Decode_Modrm_Mem (Off : Pc_Type; Width : Width_Type)
      is
         B : Byte;
         B_Mod : Bf_2;
         B_Rm : Bf_3;
      begin
         B := Mem (Off);
         B_Mod := Ext_Modrm_Mod (B);
         B_Rm := Ext_Modrm_Rm (B);
         case B_Mod is
            when 2#11# =>
               Decode_Reg_Field (B_Rm, Width);
            when 2#10# =>
               if B_Rm = 2#100# then
                  Decode_Disp (Off + 2, W_32);
                  Decode_Sib (Mem (Off + 1), B_Mod);
               else
                  Decode_Disp (Off + 1, W_32);
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
               end if;
            when 2#01# =>
               if B_Rm = 2#100# then
                  Decode_Disp (Off + 2, W_8);
                  Decode_Sib (Mem (Off + 1), B_Mod);
               else
                  Decode_Disp (Off + 1, W_8);
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
               end if;
            when 2#00# =>
               if B_Rm = 2#100# then
                  B := Mem (Off + 1);
                  if Ext_Sib_Base (B) = 2#101# then
                     Decode_Disp (Off + 2, W_32);
                  end if;
                  Decode_Sib (B, B_Mod);
               elsif B_Rm = 2#101# then
                  Decode_Disp (Off + 1, W_32);
               else
                  Add_Char ('(');
                  Decode_Reg_Field (B_Rm, W_32);
                  Add_Char (')');
               end if;
         end case;
      end Decode_Modrm_Mem;

      --  Return the length of the modrm bytes.
      --  At least 1 (mod/rm), at most 6 (mod/rm + SUB + disp32).
      function Decode_Modrm_Len (Off : Pc_Type) return Pc_Type
      is
         B : Byte;
         M_Mod : Bf_2;
         M_Rm : Bf_3;
      begin
         B := Mem (Off);
         M_Mod := Ext_Modrm_Mod (B);
         M_Rm := Ext_Modrm_Rm (B);
         case M_Mod is
            when 2#11# =>
               --  Register.
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
                  --  disp32.
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

      procedure Add_Operand (C : Code_Type;
                             Off_Modrm : Pc_Type;
                             Off_Imm : in out Pc_Type;
                             W : Width_Type) is
      begin
         case C is
            when C_Reg_Bp =>
               Add_String ("%ebp");
            when C_Reg_Ax =>
               Add_String ("%eax");
            when C_Reg_Dx =>
               Add_String ("%edx");
            when C_Reg_Cx =>
               Add_String ("%ecx");
            when C_Reg_Bx =>
               Add_String ("%ebx");
            when C_Reg_Si =>
               Add_String ("%esi");
            when C_Reg_Di =>
               Add_String ("%edi");
            when C_Reg_Sp =>
               Add_String ("%esp");
            when C_Reg_Al =>
               Add_String ("%al");
            when C_Reg_Cl =>
               Add_String ("%cl");
            when C_Reg_Bl =>
               Add_String ("%bl");
            when C_Reg_Ah =>
               Add_String ("%ah");
            when C_Gv =>
               Decode_Modrm_Reg (Mem (Off_Modrm), W);
            when C_Gv_Ib =>
               Decode_Imm (Off_Imm, W_8);
               Add_Comma;
               Decode_Modrm_Reg (Mem (Off_Modrm), W);
            when C_Gb =>
               Decode_Modrm_Reg (Mem (Off_Modrm), W_8);
            when C_Ev =>
               Decode_Modrm_Mem (Off_Modrm, W);
            when C_Ew =>
               Decode_Modrm_Mem (Off_Modrm, W_32);
            when C_Ev_Ib =>
               Decode_Imm (Off_Imm, W_8);
               Add_Comma;
               Decode_Modrm_Mem (Off_Modrm, W);
            when C_Ev_Iz =>
               Decode_Imm (Off_Imm, W_32); -- FIXME: oper16
               Add_Comma;
               Decode_Modrm_Mem (Off_Modrm, W);
            when C_M | C_Mfs | C_Mfd | C_Mfe | C_Mq =>
               Decode_Modrm_Mem (Off_Modrm, W_None);
            when C_Eb =>
               Decode_Modrm_Mem (Off_Modrm, W_8);
            when C_Ib =>
               Decode_Imm (Off_Imm, W_8);
            when C_Iv =>
               Decode_Imm (Off_Imm, W);
            when C_Iz =>
               Decode_Imm (Off_Imm, W_32); -- FIXME: oper16
            when C_Jz =>
               Decode_Disp_Rel (Off_Imm, W_32);  --  FIXME: oper16
            when C_Jb =>
               Decode_Disp_Rel (Off_Imm, W_8);
            when C_Ov | C_Ob =>
               Decode_Imm (Off_Imm, W_32); --  FIXME: oper16
            when C_Yb =>
               Add_String ("%es:(%edi)");
            when C_Yv =>
               Add_String ("%es:(");
               Decode_Reg_Field (7, W);
               Add_Char (')');
            when C_Xv =>
               Add_String ("%ds:(");
               Decode_Reg_Field (6, W);
               Add_Char (')');
            when C_Xb =>
               Add_String ("%ds:(%esi)");
            when C_H =>
               Decode_Reg_St (Ext_Modrm_Rm (Mem (Off_Modrm)));
            when C_H0 =>
               Decode_Reg_St (0);
            when C_Cst_1 =>
               Add_String ("1");
            when others =>
               raise Program_Error with
                 "unhandled x86 code_type " & Code_Type'Image (C);
         end case;
      end Add_Operand;

      Off : Pc_Type;
      Off_Modrm : Pc_Type;
      Off_Imm : Pc_Type;
      B : Byte;
      B1 : Byte;
      Desc : Insn_Desc_Type;
      Name : String8;
      W : Width_Type := W_32;
      Src, Dst : Code_Type;
   begin
      Off := Insn_Bin'First;
      Lo := Line'First;

      B := Mem (Off);
      if B = 16#66# then
         Off := Off + 1;
         B := Mem (Off);
         W := W_16;
      end if;

      if B = 16#0F# then
         B := Mem (Off + 1);
         Off := Off + 2;
         Desc := Insn_Desc_0F (B);
      else
         Off := Off + 1;
         Desc := Insn_Desc (B);
         case Desc.Dst is
            when C_Prefix_Rep =>
               B1 := Mem (Off);
               if B1 = 16#0F# then
                  raise Program_Error with "disa_x86: mmx/xmm unhandled";
               end if;
               Add_String (Desc.Name);
               Off := Off + 1;
               Desc := Insn_Desc (B1);
            when others =>
               null;
         end case;
      end if;

      --  TODO.
      if Desc.Src = C_Prefix then
         raise Program_Error;
      end if;

      case Desc.Name (1) is
         when ' ' =>
            Name := "invalid*";
            Src := C_None;
            Dst := C_None;
         when '1' =>
            B1 := Mem (Off);
            Name (1 .. 3) := Group_Name_1 (Ext_543 (B1));
            Name (4) := ' ';
            Src := Desc.Src;
            Dst := Desc.Dst;
         when '2' =>
            B1 := Mem (Off);
            Name (1 .. 3) := Group_Name_2 (Ext_543 (B1));
            Name (4) := ' ';
            Src := Desc.Src;
            Dst := Desc.Dst;
         when '3' =>
            B1 := Mem (Off);
            Dst := Desc.Dst;
            Desc := Insn_Desc_G3 (Ext_543 (B1));
            Name := Desc.Name;
            if B = 16#F6# then
               Src := Desc.Dst;
            else
               Src := Desc.Src;
            end if;
         when '5' =>
            B1 := Mem (Off);
            Desc := Insn_Desc_G5 (Ext_543 (B1));
            Name := Desc.Name;
            Src := Desc.Src;
            Dst := Desc.Dst;
         when 'E' =>
            Name (1) := 'E';
            Src := C_M;
            Dst := C_None;
         when 'a' .. 'z' =>
            Name := Desc.Name;
            Src := Desc.Src;
            Dst := Desc.Dst;
         when others =>
            raise Program_Error with "disa_x86 unhandled name " & Desc.Name;
      end case;

      Off_Modrm := Off;
      if Src in Modrm_Code or else Dst in Modrm_Code then
         Off_Imm := Off_Modrm + Decode_Modrm_Len (Off_Modrm);
      else
         Off_Imm := Off_Modrm;
      end if;

      if Name (1) = 'E' then
         B1 := Mem (Off);
         if Ext_Modrm_Mod (B1) /= 2#11# then
            Desc := Insn_Desc_Esc (Bf_3 (B and 2#111#), Ext_Modrm_Reg (B1));
            Dst := Desc.Dst;
            Src := C_None;
            Name := Desc.Name;
         else
            case Bf_3 (B and 2#111#) is
               when 2#000# =>
                  Desc := Insn_Desc_Esc_D8 (Ext_Modrm_Reg (B1));
               when 2#001# =>
                  Desc := Insn_Desc_Esc_D9 (Bf_6 (B1 and 2#111111#));
               when 2#010# =>
                  Desc := Insn_Desc_Esc_DA (Bf_6 (B1 and 2#111111#));
               when 2#101# =>
                  Desc := Insn_Desc_Esc_DD (Ext_Modrm_Reg (B1));
               when 2#110# =>
                  Desc := Insn_Desc_Esc_DE (Bf_6 (B1 and 2#111111#));
               when 2#111# =>
                  Desc := Insn_Desc_Esc_DF (Bf_6 (B1 and 2#111111#));
               when others =>
                  raise Program_Error with "disa_x86: unhandled esc 00-bf";
            end case;
            Dst := Desc.Dst;
            Src := Desc.Src;
            Name := Desc.Name;
         end if;
      end if;

      Add_Name (Name);
      Name_Align (Line'First);

      if Src /= C_None then
         Add_Operand (Src, Off_Modrm, Off_Imm, W);
         Add_Comma;
      end if;
      if Dst /= C_None then
         Add_Operand (Dst, Off_Modrm, Off_Imm, W);
      end if;

      Line_Pos := Lo;
      Insn_Len := Natural (Off_Imm - Mem'First);
   end Disassemble_Insn;

   function Get_Insn_Length
     (Self     : X86_Disassembler;
      Insn_Bin : Binary_Content) return Positive is
   begin
      raise Program_Error with "not implemented";
      return 1;
   end Get_Insn_Length;

   procedure Get_Insn_Properties
     (Self       : X86_Disassembler;
      Insn_Bin   : Binary_Content;
      Pc         : Pc_Type;
      Branch     : out Branch_Kind;
      Flag_Indir : out Boolean;
      Flag_Cond  : out Boolean;
      Dest       : out Pc_Type) is
   begin
      raise Program_Error with "not implemented";
   end Get_Insn_Properties;

end Disa_X86;
