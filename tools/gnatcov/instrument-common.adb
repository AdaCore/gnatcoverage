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

with Ada.Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Unchecked_Deallocation;

with Outputs; use Outputs;
with Project;

package body Instrument.Common is

   use Ada.Strings.Unbounded, Libadalang.Analysis, Libadalang.Rewriting;

   function To_Compilation_Unit_Name
     (Unit_Name : String;
      Unit_Part : Unit_Parts) return Compilation_Unit_Name;
   --  Create a Compilation_Unit_Name from its two components (name of
   --  library unit or subunit, and part type).

   function Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name;
      Buffer_Name       : String) return String;
   --  Helper for Statement_Buffer_Symbol and Decision_Buffer_Symbol. Return
   --  the name of the symbol for the entity that contains the address of a
   --  coverage buffer for Instrumented_Unit.

   function Register_New_File
     (Info : in out Project_Info; Name : String) return String;
   --  Helper for Create_File and Start_Rewriting: compute the path to the file
   --  to create and register it to Info.Instr_Files.

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

   -----------
   -- Image --
   -----------

   function Image (CU_Name : Compilation_Unit_Name) return String is
   begin
      return To_Ada (CU_Name.Unit)
        & " "
        & (case CU_Name.Part is
              when Unit_Spec     => "spec",
              when Unit_Body     => "body",
              when Unit_Separate => "subunit");
   end Image;

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

   ----------------------------
   -- Instrumented_Unit_Slug --
   ----------------------------

   function Instrumented_Unit_Slug
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Identifier
   is
      First : Boolean := True;
   begin
      return Result : Ada_Identifier do
         --  Add a single letter so that the spec and body of the same unit
         --  don't conflict.

         Append (Result, (case Instrumented_Unit.Part is
                          when Unit_Spec     => 'S',
                          when Unit_Body     => 'B',
                          when Unit_Separate => 'S'));
         Append (Result, '_');

         --  Create a unique suffix corresponding to the qualified name of the
         --  unit to instrument. Replace occurences of 'z' with 'zz' and insert
         --  '_z_' between identifiers.

         for Id of Instrumented_Unit.Unit loop
            if First then
               First := False;
            else
               Append (Result, "_z_");
            end if;
            for I in 1 .. Length (Id) loop
               declare
                  Char : constant Character := Element (Id, I);
               begin
                  if Char in 'Z' | 'z' then
                     Append (Result, "zz");
                  else
                     Append (Result, Char);
                  end if;
               end;
            end loop;
         end loop;
      end return;
   end Instrumented_Unit_Slug;

   -------------------
   -- Buffer_Symbol --
   -------------------

   function Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name;
      Buffer_Name       : String) return String
   is
      Slug : constant Ada_Identifier := Instrumented_Unit_Slug
        (Instrumented_Unit);
   begin
      return "xcov__buf_" & Buffer_Name & "__" & To_String (Slug);
   end Buffer_Symbol;

   -----------------------------
   -- Statement_Buffer_Symbol --
   -----------------------------

   function Statement_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "stmt");
   end Statement_Buffer_Symbol;

   ----------------------------
   -- Decision_Buffer_Symbol --
   ----------------------------

   function Decision_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "dc");
   end Decision_Buffer_Symbol;

   ------------------------
   -- MCDC_Buffer_Symbol --
   ------------------------

   function MCDC_Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name) return String is
   begin
      return Buffer_Symbol (Instrumented_Unit, "mcdc");
   end MCDC_Buffer_Symbol;

   -----------------
   -- Buffer_Unit --
   -----------------

   function Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name
   is
      Simple_Name : Ada_Identifier;
   begin
      Append (Simple_Name, 'B');
      Append (Simple_Name, Instrumented_Unit_Slug (Instrumented_Unit));
      return CU_Name : Ada_Qualified_Name := Sys_Buffers do
         CU_Name.Append (Simple_Name);
      end return;
   end Buffer_Unit;

   ----------------------
   -- Pure_Buffer_Unit --
   ----------------------

   function Pure_Buffer_Unit
     (Instrumented_Unit : Compilation_Unit_Name) return Ada_Qualified_Name
   is
      Simple_Name : Ada_Identifier;
   begin
      Append (Simple_Name, 'P');
      Append (Simple_Name, Instrumented_Unit_Slug (Instrumented_Unit));
      return CU_Name : Ada_Qualified_Name := Sys_Buffers do
         CU_Name.Append (Simple_Name);
      end return;
   end Pure_Buffer_Unit;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self           : out Source_Rewriter;
      Info           : in out Project_Info;
      Input_Filename : String)
   is
      Base_Filename   : constant String :=
         Ada.Directories.Simple_Name (Input_Filename);
      Output_Filename : constant String :=
         To_String (Info.Output_Dir) / Base_Filename;

      Context : constant Analysis_Context := Create_Context;
      Unit    : constant Analysis_Unit :=
         Get_From_File (Context, Input_Filename);
   begin
      if Unit.Has_Diagnostics then
         Error ("instrumentation failed for " & Input_Filename);
         Error ("please make sure the original project can be compiled");
         for D of Unit.Diagnostics loop
            Error (Unit.Format_GNU_Diagnostic (D));
         end loop;
         raise Xcov_Exit_Exc;
      end if;

      Self.Input_Filename := To_Unbounded_String (Input_Filename);
      Self.Output_Filename := To_Unbounded_String (Output_Filename);
      Self.Context := Context;
      Self.Unit := Unit;
      Self.Handle := Start_Rewriting (Context);

      Info.Instr_Files.Insert (To_Unbounded_String (Base_Filename));
   end Start_Rewriting;

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
            Error ("instrumentation failed for "
                   & To_String (Self.Input_Filename));
            Error ("this is likely a bug in GNATcoverage: please report it");
            for D of Result.Diagnostics loop
               Error (Self.Unit.Format_GNU_Diagnostic (D));
            end loop;
         end if;
      end;

      Self.Finalize;
      if Has_Error then
         raise Xcov_Exit_Exc;
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
     (CSS : access Checkpoints.Checkpoint_Save_State) is
   begin
      Instrumented_Unit_To_CU_Maps.Map'Write
        (CSS.Stream, Instrumented_Unit_CUs);
   end Checkpoint_Save;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : access Checkpoints.Checkpoint_Load_State)
   is
      use Instrumented_Unit_To_CU_Maps;

      CP_IU_Map : Map;
   begin
      Map'Read (CLS.Stream, CP_IU_Map);

      for Cur in CP_IU_Map.Iterate loop
         declare
            CP_Unit_Name : constant Compilation_Unit_Name := Key (Cur);

            Existing_Cur   : constant Cursor :=
              Instrumented_Unit_CUs.Find (CP_Unit_Name);
            Existing_CU_Id : constant CU_Id :=
              (if Existing_Cur = No_Element
               then No_CU_Id
               else Element (Existing_Cur));
            New_CU_Id      : constant CU_Id := CLS.CU_Map (Element (Cur));

         begin
            if Existing_CU_Id = No_CU_Id then
               Instrumented_Unit_CUs.Insert (CP_Unit_Name, New_CU_Id);

            elsif Existing_CU_Id /= New_CU_Id then
               Warn ("inconsistent information for instrumented unit "
                     & Image (CP_Unit_Name));
            end if;
         end;
      end loop;
   end Checkpoint_Load;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context (Auto_Dump_Buffers : Boolean) return Inst_Context is
   begin
      return IC : Inst_Context do
         IC.Project_Name := +Ada.Directories.Base_Name
           (Project.Root_Project_Filename);
         --  TODO??? Get the original casing for the project name

         IC.Auto_Dump_Buffers := Auto_Dump_Buffers;
      end return;
   end Create_Context;

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Context : in out Inst_Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Project_Info, Project_Info_Access);
   begin
      --  Deallocate all Project_Info in Context, and then clear the hashed
      --  map, both to avoid dangling pointers and to make Destroy_Context
      --  callable more than once, like conventional deallocation procedures in
      --  Ada.

      for Cur in Context.Project_Info_Map.Iterate loop
         declare
            PI : Project_Info_Access := Project_Info_Maps.Element (Cur);
         begin
            Free (PI);
         end;
      end loop;
      Context.Project_Info_Map := Project_Info_Maps.Empty_Map;
   end Destroy_Context;

   --------------------------------
   -- Get_Or_Create_Project_Info --
   --------------------------------

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access
   is
      use type GNATCOLL.VFS.Filesystem_String;
      use Project_Info_Maps;

      --  Look for an existing Project_Info record corresponding to Project

      Project_Name : constant Unbounded_String := +Project.Name;
      Position     : constant Cursor := Context.Project_Info_Map.Find
        (Project_Name);
   begin
      if Has_Element (Position) then
         return Element (Position);

      else
         --  The requested Project_Info record does not exist yet. Create it,
         --  register it and return it.

         declare
            Project_Obj_Dir : constant String := +Project.Object_Dir.Full_Name;
            Result          : constant Project_Info_Access := new Project_Info'
              (Output_Dir  => +(Project_Obj_Dir / "gnatcov-instr"),
               Instr_Files => <>);
         begin
            Context.Project_Info_Map.Insert (Project_Name, Result);
            return Result;
         end;
      end if;
   end Get_Or_Create_Project_Info;

   ---------------------------------
   -- Register_Main_To_Instrument --
   ---------------------------------

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type)
   is
      File_Info : constant GNATCOLL.Projects.File_Info :=
         Standard.Project.Project.Info (File);
      CU_Name   : constant Compilation_Unit_Name :=
         To_Compilation_Unit_Name (File_Info);
   begin
      --  If this main is already a unit of interest, no need to register it:
      --  we will instrument it as part of our regular instrumentation process.

      if Context.Instrumented_Units.Contains (CU_Name) then
         return;
      end if;

      declare
         Prj_Info  : constant Project_Info_Access :=
            Get_Or_Create_Project_Info (Context, Project);
      begin
         Context.Main_To_Instrument_Vector.Append
           ((Unit     => CU_Name.Unit,
             File     => File,
             Prj_Info => Prj_Info));
      end;
   end Register_Main_To_Instrument;

   -----------------------
   -- Register_New_File --
   -----------------------

   function Register_New_File
     (Info : in out Project_Info; Name : String) return String
   is
      Base_Filename   : constant String :=
         Ada.Directories.Simple_Name (Name);
      Output_Filename : constant String :=
         To_String (Info.Output_Dir) / Base_Filename;
   begin
      Info.Instr_Files.Insert (To_Unbounded_String (Base_Filename));
      return Output_Filename;
   end Register_New_File;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File
     (Info : in out Project_Info;
      File : in out Text_Files.File_Type;
      Name : String)
   is
      Filename : constant String := Register_New_File (Info, Name);
   begin
      File.Create (Filename);
   end Create_File;

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
   Sys_Prefix.Append (To_Unbounded_String ("GNATcov_RTS"));

   Sys_Buffers := Sys_Prefix;
   Sys_Buffers.Append (To_Unbounded_String ("Buffers"));

   Sys_Buffers_Lists := Sys_Buffers;
   Sys_Buffers_Lists.Append (To_Unbounded_String ("Lists"));

   Statement_Buffer_Name.Append (To_Unbounded_String ("Statement_Buffer"));
   Decision_Buffer_Name.Append  (To_Unbounded_String ("Decision_Buffer"));
   MCDC_Buffer_Name.Append      (To_Unbounded_String ("MCDC_Buffer"));

end Instrument.Common;
