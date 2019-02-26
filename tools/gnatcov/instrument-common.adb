------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Characters.Handling;

with Project;

package body Instrument.Common is

   function To_Compilation_Unit_Name
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return Compilation_Unit_Name;
   --  Create a Compilation_Unit_Name from its two components (name of
   --  library unit or subunit, and part type).

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Name : Ada_Qualified_Name) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Id of Name loop
         if Length (Result) > 0 then
            Append (Result, ".");
         end if;
         Append (Result, To_String (Id));
      end loop;

      return +Result;
   end To_Ada;

   ------------------------------
   -- To_Compilation_Unit_Name --
   ------------------------------

   function To_Compilation_Unit_Name
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return Compilation_Unit_Name
   is
      First : Positive := Unit_Name'First;
   begin
      return Result : Compilation_Unit_Name do
         --  Split Ada qualified name into its components

         for J in Unit_Name'First .. Unit_Name'Last + 1 loop
            if J = Unit_Name'Last + 1 or else Unit_Name (J) = '.' then
               Result.Unit.Append
                 (To_Unbounded_String (Unit_Name (First .. J - 1)));
               First := J + 1;
            end if;
         end loop;

         Result.Part := Unit_Part;
      end return;
   end To_Compilation_Unit_Name;

   function To_Compilation_Unit_Name
     (Source_File : GNATCOLL.Projects.File_Info) return Compilation_Unit_Name
   is
   begin
      return To_Compilation_Unit_Name
        (Source_File.Unit_Name, Source_File.Unit_Part);
   end To_Compilation_Unit_Name;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename (CU_Name : Compilation_Unit_Name) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Id of CU_Name.Unit loop
         if Length (Result) > 0 then
            Append (Result, "-");
         end if;
         Append (Result, Ada.Characters.Handling.To_Lower (To_String (Id)));
      end loop;

      case CU_Name.Part is
         when Unit_Spec =>
            Append (Result, ".ads");
         when Unit_Body | Unit_Separate =>
            Append (Result, ".adb");
      end case;

      return +Result;
   end To_Filename;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Compilation_Unit_Name) return Boolean is
   begin
      for I in 1 .. Left.Unit.Last_Index loop

         if I > Right.Unit.Last_Index then

            --  Here, we know that Left.Unit and Right.Unit are equal up to
            --  Right.Unit's last index. Left is longer, so it comes after
            --  Right.

            return False;
         end if;

         declare
            Left_Id  : constant Ada_Identifier := Left.Unit (I);
            Right_Id : constant Ada_Identifier := Right.Unit (I);
         begin
            if Left_Id < Right_Id then
               return True;
            elsif Left_Id > Right_Id then
               return False;
            end if;

            --  Here, Left.Unit and Right.Unit are equal up to I. Continue
            --  looking for differences.
         end;
      end loop;

      --  If Left is longer than Right, the return statement in the loop above
      --  has bee executed. So at this point Left is either shorter or have the
      --  same length than Right, and we know that Left is Right's prefix. So:
      --
      --  * either they have the same length: proceed to compare the unit kind
      --  * either not (right is bigger) and Left comes first.

      if Left.Unit.Last_Index /= Right.Unit.Last_Index then
         return True;
      end if;

      return Left.Part < Right.Part;
   end "<";

   -----------------
   -- Buffer_Unit --
   -----------------

   function Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name
   is
   begin
      return CU_Name : Ada_Qualified_Name := Sys_Buffers do
         CU_Name.Append
           (case Instrumented_Unit.Part is
               when Unit_Spec     => To_Unbounded_String ("Specs"),
               when Unit_Body     => To_Unbounded_String ("Bodies"),
               when Unit_Separate => To_Unbounded_String ("Subunits"));

         --  Create a unique identifier corresponding to the qualified name of
         --  the unit to instrument. Replace occurences of 'z' with 'zz' and
         --  insert '_z_' between identifiers.

         declare
            Simple_Name : Ada_Identifier;
         begin
            for Id of Instrumented_Unit.Unit loop
               if Length (Simple_Name) > 0 then
                  Append (Simple_Name, "_z_");
               end if;
               for I in 1 .. Length (Id) loop
                  declare
                     Char : constant Character := Element (Id, I);
                  begin
                     if Char in 'Z' | 'z' then
                        Append (Simple_Name, "zz");
                     else
                        Append (Simple_Name, Char);
                     end if;
                  end;
               end loop;
            end loop;
            CU_Name.Append (Simple_Name);
         end;
      end return;
   end Buffer_Unit;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS : in out Checkpoints.Checkpoint_Save_State) is
   begin
      Instrumented_Unit_Maps.Map'Write (CSS.Stream, Instrumented_Units);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : in out Checkpoints.Checkpoint_Load_State)
   is
      use Instrumented_Unit_Maps;

      CP_IU_Map : Map;
   begin
      Map'Read (CLS.Stream, CP_IU_Map);

      for Cur in CP_IU_Map.Iterate loop
         declare
            CP_IU : constant Instrumented_Unit_Info := Element (Cur);
         begin
            Instrumented_Units.Insert
              (Key (Cur),
               Instrumented_Unit_Info'(Is_Main => CP_IU.Is_Main,
                                       CU      => CLS.CU_Map (CP_IU.CU)));
         end;
      end loop;
   end Checkpoint_Load;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context (Auto_Dump_Buffers : Boolean) return Inst_Context is
      use Ada.Strings.Unbounded;

      Output_Dir  : constant String := Project.Output_Dir / "gnatcov-instr";
      Instr_Dir   : constant String := Output_Dir / "src-instr";
      Buffers_Dir : constant String := Output_Dir / "src-buffers";
   begin
      return IC : Inst_Context do
         IC.Project_Name := +Ada.Directories.Base_Name
           (Project.Root_Project_Filename);
         --  TODO??? Get the original casing for the project name

         IC.Output_Dir := +Output_Dir;
         IC.Instr_Dir := +Instr_Dir;
         IC.Buffers_Dir := +Buffers_Dir;
         IC.Auto_Dump_Buffers := Auto_Dump_Buffers;
      end return;
   end Create_Context;

   ----------------------------
   -- Find_Instrumented_Unit --
   ----------------------------

   function Find_Instrumented_Unit
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return Instrumented_Unit_Info
   is
   begin
      return Instrumented_Units.Element
        (To_Compilation_Unit_Name (Unit_Name, Unit_Part));
   end Find_Instrumented_Unit;

begin
   Sys_Prefix.Append (To_Unbounded_String ("System"));
   Sys_Prefix.Append (To_Unbounded_String ("GNATcov"));

   Sys_Buffers := Sys_Prefix;
   Sys_Buffers.Append (To_Unbounded_String ("Buffers"));

   Sys_Buffers_Lists := Sys_Buffers;
   Sys_Buffers_Lists.Append (To_Unbounded_String ("Lists"));

   Stmt_Buffer_Name.Append (To_Unbounded_String ("Buffers"));
   Stmt_Buffer_Name.Append (To_Unbounded_String ("Stmt"));

   Decision_Buffer_Name.Append (To_Unbounded_String ("Buffers"));
   Decision_Buffer_Name.Append (To_Unbounded_String ("Dc"));
end Instrument.Common;
