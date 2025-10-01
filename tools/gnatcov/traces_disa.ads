------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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
with Logging;
with Traces;            use Traces;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Elf;        use Traces_Elf;

package Traces_Disa is

   Break_Long_Instructions_Trace : constant Logging.GNATCOLL_Trace :=
     Logging.Create_Trace ("BREAK_LONG_INSTRUCTIONS");
   --  Note that enabling this trace will break long instructions in
   --  disassemblies, a la objdump.

   function Get_Label
     (Sym : Symbolizer'Class; Info : Address_Info_Acc) return String;
   --  Return the symbol corresponding to Info, if any, or return an empty
   --  string.

   type Disassemble_Cb is
     access procedure
       (Addr     : Pc_Type;
        State    : Insn_State;
        Insn     : Binary_Content;
        Insn_Set : Insn_Set_Type;
        Sym      : Symbolizer'Class);

   procedure For_Each_Insn
     (Insns    : Binary_Content;
      I_Ranges : Insn_Set_Ranges;
      State    : Insn_State;
      Cb       : Disassemble_Cb;
      Sym      : Symbolizer'Class);
   --  Call Cb for each instruction in Insns (State and Sym are parameters to
   --  pass to Cb).

   function Disassemble
     (Insn     : Binary_Content;
      Pc       : Pc_Type;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class) return String;
   --  Generate the disassembly for Insn. Insn is expected to contain exactly
   --  one instruction: a Constraint_Error is raised otherwise. Pc is the
   --  target address of Insn, used to display branch targets.

   procedure Disp_Assembly_Lines
     (Insns    : Binary_Content;
      I_Ranges : Insn_Set_Ranges;
      Base     : Traces_Base;
      Cb       :
        access procedure
          (Addr     : Pc_Type;
           State    : Insn_State;
           Insn     : Binary_Content;
           Insn_Set : Insn_Set_Type;
           Sym      : Symbolizer'Class);
      Sym      : Symbolizer'Class);
   --  Call Cb for each instruction in Insns, computing instruction state from
   --  the Base set of traces.

   procedure Textio_Disassemble_Cb
     (Addr     : Pc_Type;
      State    : Insn_State;
      Insn     : Binary_Content;
      Insn_Set : Insn_Set_Type;
      Sym      : Symbolizer'Class);
   --  Simple callback for Disp_Assembly_Lines: emit state-annotated
   --  disassembly to the standard output.

   procedure Dump_Traces_With_Asm
     (Exe : Exe_File_Type'Class; Trace_Filename : String);
   --  Debug procedure: dump a trace file and the disassembly for each entry

end Traces_Disa;
