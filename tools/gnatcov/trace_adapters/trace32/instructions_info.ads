------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

with Elf_Disassemblers; use Elf_Disassemblers;
with Traces;            use Traces;
with Traces_Elf;        use Traces_Elf;

package Instructions_Info is

   type Insn_Info is tagged limited private;

   procedure Load_Elf (This      : in out Insn_Info;
                       Exec_Path : String);
   --  Load .text section of the given ELF binary file

   function  Loaded (This : Insn_Info) return Boolean;
   --  Return True if a .text section is loaded

   function Get_Next_Insn_Address (This : in out Insn_Info;
                                   PC   : Pc_Type)
                                   return Pc_Type
     with
       Pre => Loaded (This);
   --  Return the address of the next instruction after PC

   function Get_Insn_Length (This : in out Insn_Info;
                             PC   : Pc_Type)
                             return Pc_Type
     with
       Pre => Loaded (This);
   --  Return memory lenght of the instruction at PC

   function Is_Branch (This : in out Insn_Info;
                       PC   : Pc_Type)
                       return Boolean
     with
       Pre => Loaded (This);
   --  Return True if the instruction at PC is a branch instruction

   function Fallthrough_Address (This           : in out Insn_Info;
                                 Caller, Target : Pc_Type)
                                 return Boolean
     with
       Pre => Loaded (This) and then Is_Branch (This, Caller);
   --  Return True if Target is the fallthouth address of the branch
   --  instruction at Caller.

private
   type Insn_Info is tagged limited record
      Exec     : Exe_File_Acc := null;
      Cache    : Elf_Disassemblers.Insn_Set_Cache :=
        Elf_Disassemblers.Empty_Cache;
      Section  : Address_Info_Acc;
      I_Ranges : Elf_Disassemblers.Insn_Set_Ranges;
   end record;
end Instructions_Info;
