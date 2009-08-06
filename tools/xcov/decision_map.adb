------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
with Interfaces;

with Elf_Disassemblers; use Elf_Disassemblers;
with Hex_Images;        use Hex_Images;
with SC_Obligations;    use SC_Obligations;
with Qemu_Traces;
with Sources;           use Sources;
with Strings;           use Strings;
with Switches;          use Switches;
with Traces;            use Traces;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Elf;        use Traces_Elf;
with Traces_Files;      use Traces_Files;
with Traces_Names;      use Traces_Names;
with Types;

package body Decision_Map is

   Decision_Map_Base : Traces_Base;
   --  The decision map is a list of code addresses, so we manage it as a
   --  trace database.

   procedure Analyze (ALI_List_Filename : String);
   --  Build the decision map from the executable, debug information and
   --  the Source Coverage Obligations.

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info);
   --  Build decision map for the given subprogram

   procedure Analyze_Conditional_Branch
     (Exe  : Exe_File_Acc;
      Insn : Binary_Content);
   --  Process one conditional branch instruction: identify relevant source
   --  coverable construct, and record association in the decision map.

   -------------
   -- Analyze --
   -------------

   procedure Analyze (ALI_List_Filename : String) is
   begin
      Load_SCOs (ALI_List_Filename);
      Init_Base (Decision_Map_Base, Full_History => False);
      Traces_Names.Iterate (Analyze_Routine'Access);

      if Verbose then
         Report_SCOs_Without_Code;
      end if;
   end Analyze;

   --------------------------------
   -- Analyze_Conditional_Branch --
   --------------------------------

   procedure Analyze_Conditional_Branch
     (Exe  : Exe_File_Acc;
      Insn : Binary_Content)
   is
      use Types;

      First_Sloc, Last_Sloc : Source_Location;
      --  Source location range of Insn

      SCO : SCO_Id;
   begin
      Get_Sloc_Range (Exe.all, Insn'First, First_Sloc, Last_Sloc);

      if First_Sloc = Sources.No_Location then
         --  No associated source, so no further processing required for source
         --  coverage analysis.

         return;
      end if;

      --  Normalize source file name

      pragma Assert (First_Sloc.Source_File = Last_Sloc.Source_File);

      First_Sloc.Source_File :=
        Get_Index (Simple_Name (Get_Name (First_Sloc.Source_File)));
      Last_Sloc.Source_File := First_Sloc.Source_File;

      --  Look up SCO

      SCO := Slocs_To_SCO (First_Sloc, Last_Sloc);

      if Verbose then
         Put_Line ("cond branch at " & Hex_Image (Insn'First)
                   & " " & Image (First_Sloc) & "-" & Image (Last_Sloc)
                   & ": " & Image (SCO));

      end if;

      if SCO /= No_SCO_Id then
         case Kind (SCO) is
            when Condition =>
               --  For conditions, we need full (historical) traces in order to
               --  provide MC/DC source coverage analysis.

               Add_Entry
                 (Base  => Decision_Map_Base,
                  First => Insn'First,
                  Last  => Insn'Last,
                  Op    => 0);

               Add_Address (SCO, Insn'First);
            when others =>
               null;
         end case;
      end if;
   end Analyze_Conditional_Branch;

   ---------------------
   -- Analyze_Routine --
   ---------------------

   procedure Analyze_Routine
     (Name : String_Acc;
      Info : in out Subprogram_Info)
   is
      use type Interfaces.Unsigned_32;

      PC       : Pc_Type;
      Insn_Len : Natural;

   --  Start of processing for Analyze_Routine

   begin
      Put_Line ("Building decision map for " & Name.all);
      Build_Debug_Lines (Info.Exec.all);

      if Info.Insns = null then
         Put_Line ("No instructions for " & Name.all);
         return;
      end if;

      --  Iterate over instructions, looking for conditional branches

      PC := Info.Insns'First;
      while PC < Info.Insns'Last loop
         Insn_Len :=
           Disa_For_Machine (Machine).
             Get_Insn_Length (Info.Insns (PC .. Info.Insns'Last));

         declare
            Insn : Binary_Content renames
                     Info.Insns (PC .. PC + Pc_Type (Insn_Len) - 1);

            Branch     : Branch_Kind;
            Flag_Indir : Boolean;
            Flag_Cond  : Boolean;
            Dest       : Pc_Type;
            --  Properties of Insn

         begin
            Disa_For_Machine (Machine).Get_Insn_Properties
              (Insn_Bin   => Insn,
               Pc         => PC,
               Branch     => Branch,
               Flag_Indir => Flag_Indir,
               Flag_Cond  => Flag_Cond,
               Dest       => Dest);

            if Branch = Br_Jmp and then Flag_Cond then
               Analyze_Conditional_Branch (Info.Exec, Insn);
            end if;
         end;

         PC := PC + Pc_Type (Insn_Len);

         --  Handle case where PC wraps

         exit when PC = 0;
      end loop;
   end Analyze_Routine;

   ------------------------
   -- Build_Decision_Map --
   ------------------------

   procedure Build_Decision_Map (Exec_Name : String)
   is
      Exec : aliased Exe_File_Type;

      Text_Start : constant Pc_Type := 0;
      --  Should be a global option???

      Decision_Map_Filename     : String_Acc := null;
      Decision_Map_Suffix       : constant String := ".dmap";
      --  Decision map filename is constructed by appending the suffix to the
      --  executable image name.
   begin
      if ALI_List_Filename = null then
         return;
      end if;

      if Routine_List_Filename /= null then
         Traces_Names.Read_Routines_Name_From_Text
           (Routine_List_Filename.all);
      else
         Traces_Elf.Read_Routines_Name
           (Exec_Name,
            Exclude   => False,
            Keep_Open => False);
      end if;

      Decision_Map_Filename :=
        new String'(Exec_Name & Decision_Map_Suffix);
      Open_File (Exec, Exec_Name, Text_Start);
      Build_Sections (Exec);
      Build_Symbols (Exec'Unchecked_Access);
      Load_Code_And_Traces (Exec'Unchecked_Access, Base => null);
      Decision_Map.Analyze (ALI_List_Filename.all);
      Decision_Map.Write_Map (Decision_Map_Filename.all);
      Close_File (Exec);
   end Build_Decision_Map;

   ---------------
   -- Write_Map --
   ---------------

   procedure Write_Map (Filename : String) is
      Trace_File : Trace_File_Type;
   begin
      Create_Trace_File (Qemu_Traces.Decision_Map, Trace_File);
      Write_Trace_File (Filename, Trace_File, Decision_Map_Base);
   end Write_Map;

end Decision_Map;
