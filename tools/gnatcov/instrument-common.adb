------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2020, AdaCore                     --
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
with Ada.Strings.Wide_Wide_Unbounded;
pragma Warnings (Off, "* is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");
with Ada.Unchecked_Deallocation;

with Langkit_Support.Text;
with Libadalang.Common;
with Libadalang.Sources;

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

   procedure Remove_Warnings_And_Style_Checks_Pragmas
     (Rewriter : Source_Rewriter);
   --  Remove all Warnings/Style_Checks pragmas in Rewriter's unit

   -----------------------
   -- To_Qualified_Name --
   -----------------------

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Name) return Ada_Qualified_Name
   is
      use Libadalang.Common;
   begin
      return Result : Ada_Qualified_Name do
         case Ada_Name (Name.Kind) is
            when Ada_Dotted_Name =>
               declare
                  DN     : constant Dotted_Name := Name.As_Dotted_Name;
                  Suffix : constant Ada_Qualified_Name := To_Qualified_Name
                     (DN.F_Suffix.As_Name);
               begin
                  Result := To_Qualified_Name (DN.F_Prefix);
                  Result.Append (Suffix);
               end;

            when Ada_Single_Tok_Node =>
               declare
                  use Langkit_Support.Text;

                  --  ??? GNATCOLL.Projects does not specify how to encode
                  --  Unicode unit names as strings, so for now, assume that we
                  --  process only codepoints in the ASCII range and thus use
                  --  Langkit_Support.Text.Image.

                  Identifier : constant Ada_Identifier :=
                     To_Unbounded_String (Image (Name.Text));
               begin
                  Result.Append (Identifier);
               end;

            when others =>
               raise Constraint_Error
                  with "no qualified name for " & Name.Kind'Image & " nodes";
         end case;
      end return;
   end To_Qualified_Name;

   function To_Qualified_Name
     (Name : Libadalang.Analysis.Unbounded_Text_Type_Array)
      return Ada_Qualified_Name
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Langkit_Support.Text;
   begin
      return Result : Ada_Qualified_Name do
         for N of Name loop

            --  ??? Same limitation regarding non-ASCII characters as above

            Result.Append
              (To_Unbounded_String (Image (To_Wide_Wide_String (N))));
         end loop;
      end return;
   end To_Qualified_Name;

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize (Name : Ada_Qualified_Name) return Ada_Qualified_Name
   is
      use Ada.Characters.Handling;
   begin
      return Result : Ada_Qualified_Name := Name do
         for N of Result loop
            N := To_Unbounded_String (To_Lower (To_String (N)));
         end loop;
      end return;
   end Canonicalize;

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

   function To_Filename
     (Project : Project_Type; CU_Name : Compilation_Unit_Name) return String
   is
      Unit_Name : Unbounded_String;
   begin
      for Id of CU_Name.Unit loop
         if Length (Unit_Name) > 0 then
            Append (Unit_Name, ".");
         end if;
         Append (Unit_Name, Ada.Characters.Handling.To_Lower (To_String (Id)));
      end loop;

      declare
         use GNATCOLL.VFS;
      begin
         return +Project.File_From_Unit
           (Unit_Name       => +Unit_Name,
            Part            => CU_Name.Part,
            Language        => "Ada",
            File_Must_Exist => False);
      end;
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

         Append (Result, Part_Tags (Instrumented_Unit.Part) & '_');

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

   ------------------------
   -- Project_Output_Dir --
   ------------------------

   function Project_Output_Dir (Project : Project_Type) return String is
      use type GNATCOLL.VFS.Filesystem_String;
      Obj_Dir : constant String := +Project.Object_Dir.Full_Name;
   begin
      if Obj_Dir'Length = 0 then
         return "";
      else
         return Obj_Dir / "gnatcov-instr";
      end if;
   end Project_Output_Dir;

   ---------------------
   -- Start_Rewriting --
   ---------------------

   procedure Start_Rewriting
     (Self           : out Source_Rewriter;
      IC             : in out Inst_Context;
      Info           : in out Project_Info;
      Input_Filename : String)
   is
      Base_Filename   : constant String :=
         Ada.Directories.Simple_Name (Input_Filename);
      Output_Filename : constant String :=
         To_String (Info.Output_Dir) / Base_Filename;
      Unit            : constant Analysis_Unit :=
         Get_From_File (IC, Input_Filename);
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
      Self.Unit := Unit;
      Self.Handle := Start_Rewriting (IC.Context);

      Info.Instr_Files.Insert (To_Unbounded_String (Base_Filename));
   end Start_Rewriting;

   --------------------
   -- Rewritten_Unit --
   --------------------

   function Rewritten_Unit
     (Self : Source_Rewriter) return Libadalang.Analysis.Analysis_Unit is
   begin
      return Self.Unit;
   end Rewritten_Unit;

   ----------------------------------------------
   -- Remove_Warnings_And_Style_Checks_Pragmas --
   ----------------------------------------------

   procedure Remove_Warnings_And_Style_Checks_Pragmas
     (Rewriter : Source_Rewriter)
   is
      use Libadalang.Common;
      use Libadalang.Sources;

      function Should_Remove (Node : Node_Rewriting_Handle) return Boolean;
      --  Return whether Node is a pragma Warnings or Style_Checks

      procedure Process (Node : Node_Rewriting_Handle);
      --  Remove all pragma Warnings/Style_Checks statements from Node and its
      --  children.

      -------------------
      -- Should_Remove --
      -------------------

      function Should_Remove (Node : Node_Rewriting_Handle) return Boolean is
      begin
         if Kind (Node) /= Ada_Pragma_Node then
            return False;
         end if;

         declare
            Symbol : constant Symbolization_Result :=
               Canonicalize (Text (Child (Node, 1)));
         begin
            return (Symbol.Success
                    and then Symbol.Symbol in "warnings" | "style_checks");
         end;
      end Should_Remove;

      -------------
      -- Process --
      -------------

      procedure Process (Node : Node_Rewriting_Handle) is
      begin
         if Node = No_Node_Rewriting_Handle then
            return;
         end if;

         --  Go through all children in reverse order so that we can remove
         --  pragmas in one pass.

         for I in reverse 1 .. Children_Count (Node) loop
            declare
               Child : constant Node_Rewriting_Handle :=
                  Libadalang.Rewriting.Child (Node, I);
            begin
               if Child /= No_Node_Rewriting_Handle
                 and then Should_Remove (Child)
               then
                  Remove_Child (Node, I);
               else
                  Process (Child);
               end if;
            end;
         end loop;
      end Process;

   --  Start of processing for Remove_Warnings_And_Style_Checks_Pragmas

   begin
      Process (Handle (Rewriter.Unit.Root));
   end Remove_Warnings_And_Style_Checks_Pragmas;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : in out Source_Rewriter) is
   begin
      --  Automatically insert pragmas to disable style checks and
      --  warnings in generated code: it is not our goal to make
      --  instrumentation generate warning-free or well-formatted
      --  code.

      Remove_Warnings_And_Style_Checks_Pragmas (Self);

      declare
         use Ada.Strings.Wide_Wide_Unbounded;
         use Ada.Strings.Wide_Wide_Unbounded.Aux;

         Unit   : constant Unit_Rewriting_Handle := Handle (Self.Unit);
         Source : constant Unbounded_Wide_Wide_String := Unparse (Unit);

         --  To avoid copying the potentially big string for sources on the
         --  secondary stack (and reduce the amount of copies anyway), use the
         --  internal GNAT API to retreive the internal string access and
         --  process it by chunks.

         Source_Access : Big_Wide_Wide_String_Access;
         Length        : Natural;

         Chunk_Size : constant := 4096;
         Position   : Natural;

         Out_File : Text_Files.File_Type;
      begin
         Abort_Rewriting (Self.Handle);
         Out_File.Create (To_String (Self.Output_Filename));
         Put_Warnings_And_Style_Checks_Pragmas (Out_File);

         Get_Wide_Wide_String (Source, Source_Access, Length);
         Position := Source_Access.all'First;

         while Position <= Length loop
            declare
               Chunk_First : constant Natural := Position;
               Chunk_Last  : constant Natural := Natural'Min
                 (Chunk_First + Chunk_Size - 1, Length);

               Chunk         : Wide_Wide_String renames
                  Source_Access.all (Chunk_First .. Chunk_Last);
               Encoded_Chunk : constant String :=
                  Ada.Characters.Conversions.To_String (Chunk);
            begin
               Out_File.Put (Encoded_Chunk);
               Position := Chunk_Last + 1;
            end;
         end loop;
      end;

      Self.Finalize;
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

   ----------------------
   -- Checkpoint_Clear --
   ----------------------

   procedure Checkpoint_Clear is
   begin
      Instrumented_Unit_CUs.Clear;
   end Checkpoint_Clear;

   ---------------------
   -- Checkpoint_Load --
   ---------------------

   procedure Checkpoint_Load
     (CLS : access Checkpoints.Checkpoint_Load_State)
   is
      use Instrumented_Unit_To_CU_Maps;

      CP_IU_Map : Map;
      Relocs    : Checkpoints.Checkpoint_Relocations renames CLS.Relocations;
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
            New_CU_Id      : constant CU_Id := Relocs.CU_Map (Element (Cur));

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

   function Create_Context
     (Provider             : Libadalang.Analysis.Unit_Provider_Reference;
      Dump_Trigger         : Any_Dump_Trigger;
      Dump_Channel         : Any_Dump_Channel;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context is
   begin
      return IC : Inst_Context do
         IC.Project_Name := +Ada.Directories.Base_Name
           (Project.Root_Project_Filename);
         --  TODO??? Get the original casing for the project name

         IC.Provider := Provider;
         IC.Context := Libadalang.Analysis.Create_Context
           (Unit_Provider => Provider);
         IC.Get_From_File_Count := 0;

         IC.Dump_Trigger := Dump_Trigger;
         IC.Dump_Channel := Dump_Channel;
         IC.Language_Version := Language_Version;

         IC.Ignored_Source_Files_Present := Ignored_Source_Files /= null;
         if Ignored_Source_Files /= null then
            IC.Ignored_Source_Files := Ignored_Source_Files.all;
         end if;
      end return;
   end Create_Context;

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Context : in out Inst_Context) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Instrumented_Unit_Info, Instrumented_Unit_Info_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Project_Info, Project_Info_Access);
   begin
      --  Deallocate all Insrtrumented_Unit_Info in Context, and then clear the
      --  hashed map, both to avoid dangling pointers and to make
      --  Destroy_Context callable more than once, like conventional
      --  deallocation procedures in Ada.

      for Cur in Context.Instrumented_Units.Iterate loop
         declare
            IU : Instrumented_Unit_Info_Access :=
               Instrumented_Unit_Maps.Element (Cur);
         begin
            Free (IU);
         end;
      end loop;
      Context.Instrumented_Units := Instrumented_Unit_Maps.Empty_Map;

      --  Likewise for Project_Info records

      for Cur in Context.Project_Info_Map.Iterate loop
         declare
            PI : Project_Info_Access := Project_Info_Maps.Element (Cur);
         begin
            Free (PI);
         end;
      end loop;
      Context.Project_Info_Map := Project_Info_Maps.Empty_Map;
   end Destroy_Context;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (IC       : in out Inst_Context;
      Filename : String) return Libadalang.Analysis.Analysis_Unit
   is
   begin
      --  If we exceeded the maximum number of calls to Get_From_File, start
      --  with a new context.

      if IC.Get_From_File_Count >= Max_Get_From_File_Count then
         IC.Context := Create_Context (Unit_Provider => IC.Provider);
         IC.Get_From_File_Count := 0;
      end if;
      IC.Get_From_File_Count := IC.Get_From_File_Count + 1;

      return IC.Context.Get_From_File (Filename);
   end Get_From_File;

   ----------------------------
   -- Is_Ignored_Source_File --
   ----------------------------

   function Is_Ignored_Source_File
     (Context : Inst_Context; Filename : String) return Boolean
   is
   begin
      return
         Context.Ignored_Source_Files_Present
         and then GNAT.Regexp.Match (Filename, Context.Ignored_Source_Files);
   end Is_Ignored_Source_File;

   --------------------------------
   -- Get_Or_Create_Project_Info --
   --------------------------------

   function Get_Or_Create_Project_Info
     (Context : in out Inst_Context;
      Project : Project_Type) return Project_Info_Access
   is
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
            Result : constant Project_Info_Access := new Project_Info'
              (Project          => Project,
               Externally_Built => Project.Externally_Built,
               Output_Dir       => +Project_Output_Dir (Project),
               Instr_Files      => <>);
         begin
            Context.Project_Info_Map.Insert (Project_Name, Result);
            return Result;
         end;
      end if;
   end Get_Or_Create_Project_Info;

   ---------------
   -- Unit_Info --
   ---------------

   function Unit_Info
     (CU_Name : Compilation_Unit_Name;
      Info    : out GNATCOLL.Projects.File_Info) return Boolean
   is
      Prj  : Project_Type renames Project.Project.Root_Project;
      File : constant GNATCOLL.VFS.Filesystem_String := Prj.File_From_Unit
        (Unit_Name => To_Ada (CU_Name.Unit),
         Part      => CU_Name.Part,
         Language  => "Ada");
   begin
      if File'Length = 0 then
         return False;
      end if;

      Info := Prj.Create_From_Project (File);
      return True;
   end Unit_Info;

   ---------------------------------
   -- Register_Main_To_Instrument --
   ---------------------------------

   procedure Register_Main_To_Instrument
     (Context : in out Inst_Context;
      Mains   : in out Main_To_Instrument_Vectors.Vector;
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
         Mains.Append
           ((Unit     => CU_Name.Unit,
             File     => File,
             Prj_Info => Prj_Info));
      end;
   end Register_Main_To_Instrument;

   ---------------------------
   -- Add_Instrumented_Unit --
   ---------------------------

   procedure Add_Instrumented_Unit
     (Context     : in out Inst_Context;
      Project     : GNATCOLL.Projects.Project_Type;
      Source_File : GNATCOLL.Projects.File_Info)
   is
      use GNATCOLL.VFS;

      SF_Basename : constant Filesystem_String := Source_File.File.Base_Name;
   begin
      pragma Assert (not (Is_Ignored_Source_File (Context, +SF_Basename)));

      declare
         CU_Name : constant Compilation_Unit_Name :=
           To_Compilation_Unit_Name (Source_File);
      begin
         --  If we already planned to instrument this unit, do nothing more

         if Context.Instrumented_Units.Contains (CU_Name) then
            return;
         end if;

         declare
            Unit_Info : constant Instrumented_Unit_Info_Access :=
               new Instrumented_Unit_Info'
                 (Filename => To_Unbounded_String
                                (+Source_File.File.Full_Name),
                  Prj_Info => Get_Or_Create_Project_Info (Context, Project),
                  Is_Main  => GNATCOLL.Projects.Is_Main_File
                               (Project, Source_File.File.Base_Name));
         begin
            Context.Instrumented_Units.Insert (CU_Name, Unit_Info);
         end;
      end;
   end Add_Instrumented_Unit;

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

   -------------------------------------------
   -- Put_Warnings_And_Style_Checks_Pragmas --
   -------------------------------------------

   procedure Put_Warnings_And_Style_Checks_Pragmas
     (File : in out Text_Files.File_Type)
   is
   begin
      File.Put_Line ("pragma Style_Checks (Off); pragma Warnings (Off);");
   end Put_Warnings_And_Style_Checks_Pragmas;

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
