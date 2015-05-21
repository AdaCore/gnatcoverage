------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Binary_Files;      use Binary_Files;
with Disa_Symbolize;    use Disa_Symbolize;
with Elf_Disassemblers; use Elf_Disassemblers;
with Traces;            use Traces;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Elf;        use Traces_Elf;

package Traces_Disa is
   --  If True, Disp_Line_State will also display assembly code.
   Flag_Show_Asm : Boolean := False;

   --  Return the symbol for Addr followed by a colon (':').
   --  Return an empty string if none.
   function Get_Label (Sym : Symbolizer'Class; Info : Address_Info_Acc)
                      return String;

   type Disassemble_Cb is access procedure (Addr     : Pc_Type;
                                            State    : Insn_State;
                                            Insn     : Binary_Content;
                                            Insn_Set : Insn_Set_Type;
                                            Sym      : Symbolizer'Class);

   --  Call CB for each instruction of INSNS.
   --  (State and Sym are parameters of CB).
   procedure For_Each_Insn (Insns    : Binary_Content;
                            I_Ranges : Insn_Set_Ranges;
                            State    : Insn_State;
                            Cb       : Disassemble_Cb;
                            Sym      : Symbolizer'Class);

   --  Generate the disassembly for INSN.
   --  INSN is exactly one instruction.
   --  PC is the target address of INSN (used to display branch targets).
   function Disassemble
     (Insn     : Binary_Content;
      Pc       : Pc_Type;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class)
      return String;

   --  Call CB for each insn in INSNS.
   --  The state of the insn is computed.
   procedure Disp_Assembly_Lines
     (Insns    : Binary_Content;
      I_Ranges : Insn_Set_Ranges;
      Base     : Traces_Base;
      Cb       : access procedure (Addr     : Pc_Type;
                                   State    : Insn_State;
                                   Insn     : Binary_Content;
                                   Insn_Set : Insn_Set_Type;
                                   Sym      : Symbolizer'Class);
      Sym      : Symbolizer'Class);

   --  Simple callback from the previous subprogram.
   procedure Textio_Disassemble_Cb (Addr     : Pc_Type;
                                    State    : Insn_State;
                                    Insn     : Binary_Content;
                                    Insn_Set : Insn_Set_Type;
                                    Sym      : Symbolizer'Class);

   --  Debug procedure: dump a trace file and the disassembly for each entry.
   procedure Dump_Traces_With_Asm (Exe            : Exe_File_Type'Class;
                                   Trace_Filename : String);
end Traces_Disa;
