------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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
with System;
with Traces;

package Disa_Ppc is
   --  Call-back used to find a relocation symbol.
   type Symbol_Proc_Type is access procedure (Pc : Traces.Pc_Type;
                                              Line : in out String;
                                              Line_Pos : in out Natural);

   --  Return the length of the instruction at Addr.
   function Get_Insn_Length (Addr : System.Address) return Positive;
   pragma Inline (Get_Insn_Length);

   --  Disassemble instruction at ADDR, and put the result in LINE/LINE_POS.
   --  LINE_POS is the index of the next character to be written (ie line
   --   length if Line'First = 1).
   procedure Disassemble_Insn (Addr : System.Address;
                               Pc : Traces.Pc_Type;
                               Line : out String;
                               Line_Pos : out Natural;
                               Insn_Len : out Natural;
                               Proc_Cb : Symbol_Proc_Type);

   type String_Cst_Acc is access constant String;
   type Ppc_Fields is
     (
      F_AA, F_LK, F_Rc, F_OE, F_U, F_Br_Hint, F_L,

      F_A, F_B, F_C, F_D, F_S,
      F_FA, F_FB, F_FC, F_FD, F_FS,
      F_SIMM, F_UIMM,
      F_CrfD, F_CrfS, F_CrbD, F_CrbA, F_CrbB,
      F_BO, F_BI, F_BD,
      F_LI, F_SH, F_MB, F_ME, F_CRM, F_Sr, F_Tbr, F_Spr, F_NB,
      F_Disp, F_Imm, F_Fm, F_TO, F_Eof);

   --  These fields add characters to the mnemonic.
   subtype Ppc_Mnemo_Fields is Ppc_Fields range F_AA .. F_L;

   type Ppc_Fields_Arr is array (0 .. 5) of Ppc_Fields;

   type Ppc_Insn_Descr is record
      Name : String_Cst_Acc;
      Insn : Unsigned_32;
      Fields : Ppc_Fields_Arr;
   end record;


end Disa_Ppc;
