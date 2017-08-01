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

with Ada.Text_IO;
with Interfaces;         use Interfaces;

with Diagnostics;        use Diagnostics;
with Outputs;            use Outputs;
with Qemu_Traces;
with Traces;             use Traces;
with Trace_Output;       use Trace_Output;

with Instructions_Info;  use Instructions_Info;
with Trace32.Branchflow; use Trace32.Branchflow;

package body Trace32.Conversion is

   package BF renames Trace32.Branchflow;
   package Pc_Type_IO is new Ada.Text_IO.Modular_IO (Pc_Type);

   function Hex_Image (PC : Pc_Type) return String;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (PC : Pc_Type) return String is
      Ret : String (1 .. (Pc_Type'Size / 4) + 4);
   begin
      Pc_Type_IO.Put (Ret, PC, Base => 16);
      return Ret;
   end Hex_Image;

   ------------------------------
   -- Convert_Branchflow_Trace --
   ------------------------------

   procedure Convert_Branchflow_Trace
     (Elf_File          : String;
      Branchflow_File   : String;
      Qemu_Trace_File   : String;
      Decision_Map_File : String)
   is
      Insn           : Instructions_Info.Insn_Info;
      Branch_Flow    : BF.Branchflow_Trace;
      BF_Entry       : BF.Branchflow_Trace_Entry;
      BF_Status      : BF.Status_Kind;
      Prev_Landing   : Pc_Type;
      Output         : Trace_Output.QEMU_Trace_Output;
   begin

      --  Load the instruction info from the ELF file
      Insn.Load_Elf (Elf_File);

      if not Insn.Loaded then
         Outputs.Fatal_Error ("Cannot load .text section for ELF file: '" &
                                Elf_File & "'");
      end if;

      Report ("Loading Trace32 Branch Flow file: " & Branchflow_File);

      if Branch_Flow.Open (Branchflow_File) /= BF.Status_Ok
      then
         Outputs.Fatal_Error ("Cannot open Trace32 Branch Flow file: '" &
                                Branchflow_File & "'");
      end if;

      Output.Open (Qemu_Trace_File, Decision_Map_File);

      --  GNATcov execution traces work in term of basic blocks, where
      --  Branchflow traces record changes in the flow of instructions.
      --
      --  To start producing GNATcov traces we need the first address of the
      --  first basic block, we use the Target address of the first blanchflow
      --  entry.

      --  Get the branchflow first entry
      BF_Status := Branch_Flow.Next_Entry (BF_Entry);

      if BF_Status /= Status_Ok then
         --  No entry in Branchflow file
         Outputs.Fatal_Error ("No entry in Trace32 Branchflow file'" &
                                Branchflow_File & "'");
         return;
      else

         --  Use Target address of first branchflow entry as the first address
         --  of the first basic block.
         Prev_Landing := BF_Entry.Target;
      end if;

      --  Convertion loop
      loop
         BF_Status := Branch_Flow.Next_Entry (BF_Entry);

         --  Are we at the end of the Branch flow file?
         exit when BF_Status = BF.No_More_Entry;

         --  Make sure the Caller address is designating a branch instruction
         if Insn.Is_Branch (BF_Entry.Caller) then
            declare
               Ent : Traces.Trace_Entry;
            begin
               --  Set the address of the first instrunction in the basic block
               Ent.First := Prev_Landing;

               --  Set the address of the last byte of the last instruction in
               --  the basic block.
               Ent.Last  := BF_Entry.Caller +
                 Insn.Get_Insn_Length (BF_Entry.Caller) - 1;

               --  Mark the basic block as executed
               Ent.Op    := Qemu_Traces.Trace_Op_Block;

               --  Check if Target is the fallthrough address of Caller
               --  instuction.
               if Insn.Fallthrough_Address (BF_Entry.Caller,
                                            BF_Entry.Target)
               then
                  --  Fallthrough
                  Ent.Op := Ent.Op or Qemu_Traces.Trace_Op_Br1;
               else
                  --  Branch
                  Ent.Op := Ent.Op or Qemu_Traces.Trace_Op_Br0;
               end if;

               Output.Push_Entry (Ent);
            end;
         else
            Outputs.Fatal_Error ("Caller is not a branch (" &
                                   Hex_Image (BF_Entry.Caller) & ")");
         end if;

         Prev_Landing := BF_Entry.Target;
      end loop;

      Branch_Flow.Close_Trace_File;
      Output.Close_Trace_File;
   end Convert_Branchflow_Trace;

end Trace32.Conversion;
