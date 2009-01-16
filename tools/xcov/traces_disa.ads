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
with Traces; use Traces;
with Traces_Dbase; use Traces_Dbase;
with Traces_Elf; use Traces_Elf;
with Disa_Symbolize; use Disa_Symbolize;

package Traces_Disa is
   --  If True, Disp_Line_State will also display assembly code.
   Flag_Show_Asm : Boolean := False;

   --  Return the symbol for Addr followed by a colon (':').
   --  Return an empty string if none.
   function Get_Label (Sym : Symbolizer'Class; Info : Addresses_Info_Acc)
                      return String;

   type Disassemble_Cb is access procedure (Addr : Pc_Type;
                                            State : Trace_State;
                                            Insn : Binary_Content;
                                            Sym : Symbolizer'Class);

   procedure Disassemble (Insns : Binary_Content;
                          State : Trace_State;
                          Cb : Disassemble_Cb;
                          Sym : Symbolizer'Class);

   --  Generate the disassembly for INSN.
   --  INSN is exactly one instruction.
   --  PC is the target address of INSN (used to display branch targets).
   function Disassemble
     (Insn : Binary_Content; Pc : Pc_Type; Sym : Symbolizer'Class)
     return String;

   --  Call CB for each insn in INSNS.
   --  The state of the insn is computed.
   procedure Disp_Assembly_Lines
     (Insns : Binary_Content;
      Base : Traces_Base;
      Cb : access procedure (Addr : Pc_Type;
                             State : Trace_State;
                             Insn : Binary_Content;
                             Sym : Symbolizer'Class);
      Sym : Symbolizer'Class);

   --  Simple callback from the previous subprogram.
   procedure Textio_Disassemble_Cb (Addr : Pc_Type;
                                    State : Trace_State;
                                    Insn : Binary_Content;
                                    Sym : Symbolizer'Class);
end Traces_Disa;
