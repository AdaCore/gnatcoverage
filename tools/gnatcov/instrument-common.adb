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
with Ada.Characters.Conversions;

with Outputs;
with Project;
with Text_Files;

package body Instrument.Common is

   use Ada.Strings.Unbounded, Libadalang.Analysis, Libadalang.Rewriting;

   function To_Compilation_Unit_Name
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return Compilation_Unit_Name;
   --  Create a Compilation_Unit_Name from its two components (name of
   --  library unit or subunit, and part type).

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Name : Ada_Qualified_Name) return String is
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
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self            : out Source_Rewriter;
      Input_Filename  : String;
      Output_Filename : String)
   is
      Context : constant Analysis_Context := Create_Context;
      Unit    : constant Analysis_Unit :=
         Get_From_File (Context, Input_Filename);
   begin
      if Unit.Has_Diagnostics then
         Outputs.Error ("instrumentation failed for " & Input_Filename);
         Outputs.Error
           ("please make sure the original project can be compiled");
         for D of Unit.Diagnostics loop
            Outputs.Error (Unit.Format_GNU_Diagnostic (D));
         end loop;
         raise Outputs.Xcov_Exit_Exc;
      end if;

      Self.Input_Filename := To_Unbounded_String (Input_Filename);
      Self.Output_Filename := To_Unbounded_String (Output_Filename);
      Self.Context := Context;
      Self.Unit := Unit;
      Self.Handle := Start_Rewriting (Context);
   end Start_Rewriting;

   ---------------------------
   -- Start_Instr_Rewriting --
   ---------------------------

   procedure Start_Instr_Rewriting
     (Self            : out Source_Rewriter;
      IC              : in out Inst_Context;
      Input_Filename  : String)
   is
      Base_Filename   : constant String :=
         Ada.Directories.Simple_Name (Input_Filename);
      Output_Filename : constant String :=
         To_String (IC.Instr_Dir) / Base_Filename;
   begin
      Start_Rewriting (Self, Input_Filename, Output_Filename);
      IC.Instr_Files.Insert (To_Unbounded_String (Base_Filename));
   end Start_Instr_Rewriting;

   -----------------------
   -- Rewritten_Context --
   -----------------------

   function Rewritten_Context
     (Self : Source_Rewriter) return Libadalang.Analysis.Analysis_Context is
   begin
      return Self.Context;
   end Rewritten_Context;

   --------------------
   -- Rewritten_Unit --
   --------------------

   function Rewritten_Unit
     (Self : Source_Rewriter) return Libadalang.Analysis.Analysis_Unit is
   begin
      return Self.Unit;
   end Rewritten_Unit;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out Source_Rewriter) is
      Has_Error : Boolean := False;
   begin
      declare
         Result : constant Apply_Result := Apply (Self.Handle);
      begin
         if Result.Success then
            declare
               Out_File : Text_Files.File_Type;
            begin
               Out_File.Create (To_String (Self.Output_Filename));
               Out_File.Put_Line
                 (Ada.Characters.Conversions.To_String (Self.Unit.Text));
            end;

         else
            Has_Error := True;
            Outputs.Error ("instrumentation failed for "
                           & To_String (Self.Input_Filename));
            Outputs.Error
              ("this is likely a bug in GNATcoverage: please report it");
            for D of Result.Diagnostics loop
               Outputs.Error (Self.Unit.Format_GNU_Diagnostic (D));
            end loop;
         end if;
      end;

      Self.Finalize;
      if Has_Error then
         raise Outputs.Xcov_Exit_Exc;
      end if;
   end Apply;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Source_Rewriter) is
   begin
      if Self.Handle /= No_Rewriting_Handle then
         Abort_Rewriting (Self.Handle);
      end if;
      Self.Unit := No_Analysis_Unit;
      Self.Context := No_Analysis_Context;
   end Finalize;

   ---------------------
   -- Checkpoint_Save --
   ---------------------

   procedure Checkpoint_Save
     (CSS : in out Checkpoints.Checkpoint_Save_State) is
   begin
      Instrumented_Unit_To_CU_Maps.Map'Write
        (CSS.Stream, Instrumented_Unit_CUs);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : in out Checkpoints.Checkpoint_Load_State)
   is
      use Instrumented_Unit_To_CU_Maps;

      CP_IU_Map : Map;
   begin
      Map'Read (CLS.Stream, CP_IU_Map);

      for Cur in CP_IU_Map.Iterate loop
         Instrumented_Unit_CUs.Insert (Key (Cur), CLS.CU_Map (Element (Cur)));
      end loop;
   end Checkpoint_Load;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context (Auto_Dump_Buffers : Boolean) return Inst_Context is
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
      Unit_Part : Unit_Parts) return CU_Id
   is
      use Instrumented_Unit_To_CU_Maps;

      Position : constant Cursor := Instrumented_Unit_CUs.Find
        (To_Compilation_Unit_Name (Unit_Name, Unit_Part));
   begin
      if Has_Element (Position) then
         return Element (Position);
      else
         return No_CU_Id;
      end if;
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
