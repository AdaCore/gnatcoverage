------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Libadalang.Common; use Libadalang.Common;

with Outputs; use Outputs;

procedure Instrument.Find_Units
  (Context      : Libadalang.Analysis.Analysis_Context;
   CU_Name      : Compilation_Unit_Name;
   Info         : GNATCOLL.Projects.File_Info;
   Process_Unit : access procedure
     (CU_Name : Compilation_Unit_Name;
      Info    : GNATCOLL.Projects.File_Info))
is
   package LAL renames Libadalang.Analysis;
   package GPR renames GNATCOLL.Projects;

   function Process_Node (N : LAL.Ada_Node'Class) return Visit_Status;

   ------------------
   -- Process_Node --
   ------------------

   function Process_Node (N : LAL.Ada_Node'Class) return Visit_Status is
   begin
      if N.Kind in Ada_Body_Stub then
         begin
            declare
               Stub         : constant LAL.Body_Stub := N.As_Body_Stub;
               Subunit_FQN  : constant LAL.Unbounded_Text_Type_Array :=
                  Stub.P_Fully_Qualified_Name_Array;
               Subunit_Name : constant Compilation_Unit_Name :=
                 (Unit => To_Qualified_Name (Subunit_FQN),
                  Part => GPR.Unit_Separate);
               Subunit_Info : GPR.File_Info;
            begin
               if Subunit_FQN'Length = 0 then
                  raise Property_Error;
               elsif Unit_Info (Subunit_Name, Subunit_Info) then
                  Find_Units
                    (Context, Subunit_Name, Subunit_Info, Process_Unit);
               else
                  Warn ("cannot instrument " & Image (Subunit_Name)
                        & ": this unit does not belong to this project");
               end if;
            end;
         exception
            when Property_Error =>
               --  TODO: location
               Warn ("failed to locate the subunit for this stub");
         end;
         return Over;
      end if;

      return Into;
   end Process_Node;

   Input_Filename : constant String := +Info.File.Full_Name;
   Unit           : constant LAL.Analysis_Unit :=
      Context.Get_From_File (Input_Filename);

--  Start of processing for Instrument.Find_Units

begin
   --  Abort if the input project is not compilable

   if Unit.Has_Diagnostics then
      Error ("instrumentation failed for " & Input_Filename);
      Error ("please make sure the original project can be compiled");
      for D of Unit.Diagnostics loop
         Error (Unit.Format_GNU_Diagnostic (D));
      end loop;
      raise Xcov_Exit_Exc;
   end if;

   --  Abort if we have several compilation units per file

   if Unit.Root.Kind = Ada_Compilation_Unit_List then
      Error ("instrumentation failed for " & Input_Filename);
      Error ("source files containing multiple compilation units are not"
             & " supported");
      raise Xcov_Exit_Exc;
   end if;

   pragma Assert (Unit.Root.Kind = Ada_Compilation_Unit);
   declare
      CU : constant LAL.Compilation_Unit := Unit.Root.As_Compilation_Unit;
   begin
      Process_Unit (CU_Name, Info);
      CU.Traverse (Process_Node'Access);
   end;
end Instrument.Find_Units;
