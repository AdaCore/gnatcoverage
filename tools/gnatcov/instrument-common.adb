------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Ada.Characters.Conversions;
with Ada.Directories;
pragma Warnings (Off, "* is an internal GNAT unit");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "* is an internal GNAT unit");
with Ada.Unchecked_Deallocation;
with Interfaces;

with GNAT.OS_Lib;

with Langkit_Support.Symbols;
with Langkit_Support.Text;
with Libadalang.Common;
with Libadalang.Sources;

with Hex_Images;
with Outputs; use Outputs;
with Paths;   use Paths;
with Project;
with Switches;
with SCOs;

package body Instrument.Common is

   function Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name;
      Buffer_Name       : String) return String;
   --  Helper for Statement_Buffer_Symbol and Decision_Buffer_Symbol. Return
   --  the name of the symbol for the entity that contains the address of a
   --  coverage buffer for Instrumented_Unit.

   procedure Remove_Warnings_And_Style_Checks_Pragmas
     (Rewriter : Source_Rewriter);
   --  Remove all Warnings/Style_Checks pragmas in Rewriter's unit

   type Missing_Src_Reporter is new Libadalang.Analysis.Event_Handler_Interface
   with record
      Reported_Files : String_Sets.Set;
      --  Set of source file names which were already reported as missing.
      --  Libadalang does not guarantee that the Unit_Requested event is
      --  triggered only once per source, so de-duplicate events with this set.
   end record;
   --  Implementation of the Libadalang event handler interface used in
   --  Create_Missing_File_Reporter.

   overriding procedure Release (Self : in out Missing_Src_Reporter) is null;

   overriding procedure Unit_Requested_Callback
     (Self               : in out Missing_Src_Reporter;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean);
   --  If the requested unit is not found and that is an error, warn about it.
   --  Make sure we warn only once about a given source file.

   procedure Create_LAL_Context (IC : in out Inst_Context);
   --  Create a new Libadalang analysis context for IC, assigning it to
   --  IC.Context.
   --
   --  This helper takes care of passing the unit provider and the event
   --  handler that we need for all such contexts, and resets
   --  IC.Get_From_File_Count to 0, as the new context has not been used to
   --  instrument any source file yet.

   -------------------
   -- Buffer_Symbol --
   -------------------

   function Buffer_Symbol
     (Instrumented_Unit : Compilation_Unit_Name;
      Buffer_Name       : String) return String
   is
      Slug : constant String := Instrumented_Unit_Slug (Instrumented_Unit);
   begin
      return "xcov__buf_" & Buffer_Name & "__" & Slug;
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
         declare
            Prj_Name : constant String :=
               Ada.Characters.Handling.To_Lower (Project.Name);
         begin
            return Obj_Dir / Prj_Name & "-gnatcov-instr";
         end;
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
         Outputs.Error ("instrumentation failed for " & Input_Filename);
         Outputs.Error ("please make sure the original project can be "
                          & "compiled");
         for D of Unit.Diagnostics loop
            Outputs.Error (Unit.Format_GNU_Diagnostic (D));
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
      use Langkit_Support.Symbols;
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

         Filename : constant String := To_String (Self.Output_Filename);
         Out_File : Text_Files.File_Type;
      begin
         Abort_Rewriting (Self.Handle);
         Out_File.Create (Filename);
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

         Out_File.Close;
         if Switches.Pretty_Print then
            Text_Files.Run_GNATpp (Out_File);
         end if;
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

   ------------------------
   -- Create_LAL_Context --
   ------------------------

   procedure Create_LAL_Context (IC : in out Inst_Context) is
   begin
      IC.Context := Create_Context
        (Unit_Provider => IC.Provider,
         Event_Handler => IC.Event_Handler);
      IC.Get_From_File_Count := 0;
   end Create_LAL_Context;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Provider             : Libadalang.Analysis.Unit_Provider_Reference;
      Event_Handler        : Libadalang.Analysis.Event_Handler_Reference;
      Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) return Inst_Context is
   begin
      return IC : Inst_Context do
         IC.Project_Name := +Ada.Directories.Base_Name
           (Project.Root_Project_Filename);
         --  TODO??? Get the original casing for the project name

         --  Compute the tag for default source trace filenames. Use the
         --  current time as a mostly unique identifier. Put it in hexadecimal
         --  form without leading zeros to avoid too long names.

         declare
            use Interfaces;

            Time : constant Unsigned_64 :=
              Unsigned_64 (GNAT.OS_Lib.To_C (GNAT.OS_Lib.Current_Time));
            Tag  : constant String :=
               Hex_Images.Strip_Zero_Padding (Hex_Images.Hex_Image (Time));
         begin
            IC.Tag := +Tag;
         end;

         IC.Provider := Provider;
         IC.Event_Handler := Event_Handler;
         Create_LAL_Context (IC);

         IC.Dump_Config := Dump_Config;
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
         Create_LAL_Context (IC);
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
           ((CU_Name  => CU_Name,
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
                                (Project, Source_File.File.Base_Name),
                  Language => Str_To_Language (Source_File.Language));
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

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out Missing_Src_Reporter;
      Context            : Libadalang.Analysis.Analysis_Context'Class;
      Name               : Langkit_Support.Text.Text_Type;
      From               : Libadalang.Analysis.Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean)
   is
   begin
      --  We need to warn about sources that we could not find *and* whose
      --  presence is mandated by Ada.

      if Found or else not Is_Not_Found_Error then
         return;
      end if;

      --  Warn for a given source file only once, as Libadalang can invoke this
      --  callback several times. For clarity, only mention the base name,
      --  which should be unique in Ada projects anyway.

      declare
         N : constant Unbounded_String := To_Unbounded_String
           (Ada.Directories.Simple_Name (Langkit_Support.Text.Image (Name)));
      begin
         if not Self.Reported_Files.Contains (N) then
            Self.Reported_Files.Include (N);
            Warn ("Cannot find required source file: " & To_String (N));
         end if;
      end;
   end Unit_Requested_Callback;

   ----------------------------------
   -- Create_Missing_File_Reporter --
   ----------------------------------

   function Create_Missing_File_Reporter
     return Libadalang.Analysis.Event_Handler_Reference
   is
   begin
      return Create_Event_Handler_Reference
        (Missing_Src_Reporter'(others => <>));
   end Create_Missing_File_Reporter;

   ----------------
   -- Append_SCO --
   ----------------

   procedure Append_SCO
     (C1, C2             : Character;
      From, To           : Source_Location;
      Last               : Boolean;
      Pragma_Aspect_Name : Name_Id := Namet.No_Name)
   is
   begin
      SCOs.SCO_Table.Append
        ((From =>
              (Line => Logical_Line_Number (From.Line),
               Col  => Standard.Types.Column_Number (From.Column)),
          To   =>
              (Line => Logical_Line_Number (To.Line),
               Col  => Standard.Types.Column_Number (To.Column)),
          C1   => C1,
          C2   => C2,
          Last => Last,
          Pragma_Sloc        => No_Location,
          Pragma_Aspect_Name => Pragma_Aspect_Name));
   end Append_SCO;

   ------------------------------
   -- Buffer_Units_For_Closure --
   ------------------------------

   function Buffer_Units_For_Closure
     (IC   : Inst_Context;
      Main : Compilation_Unit_Name)
      return Ada_Qualified_Name_Vectors.Vector
   is
      pragma Unreferenced (Main);
      Result : Ada_Qualified_Name_Vectors.Vector;
   begin
      --  TODO??? Here, we need the list of files needed to build Main: specs
      --  for units WITHed by main, their bodies, the separates, etc.  It's
      --  unclear what GNATCOLL.Projects.Get_Closure does, but experimentations
      --  show that it's not what we want. So for now, return an approximation:
      --  buffer units for all instrumented units. In the future, we should
      --  either get this service from GNATCOLL.Projects, either re-implement
      --  it on top of Libadalang.

      for Cur in IC.Instrumented_Units.Iterate loop
         declare
            Instr_Unit : constant Compilation_Unit_Name :=
              Instrumented_Unit_Maps.Key (Cur);
         begin
            Result.Append (Buffer_Unit (Instr_Unit));
         end;
      end loop;
      return Result;

   end Buffer_Units_For_Closure;

   -------------------------------
   -- Emit_Ada_Dump_Helper_Unit --
   -------------------------------

   procedure Emit_Ada_Dump_Helper_Unit
     (IC          : Inst_Context;
      Info        : in out Project_Info;
      Main        : Compilation_Unit_Name;
      Language    : Any_Language;
      Helper_Unit : out Ada_Qualified_Name)
   is
      File : Text_Files.File_Type;

      procedure Put_With (Unit : Ada_Qualified_Name);
      --  Put a "with" context clause in File

      --------------
      -- Put_With --
      --------------

      procedure Put_With (Unit : Ada_Qualified_Name) is
      begin
         File.Put_Line ("with " & To_Ada (Unit) & ";");
      end Put_With;

      Output_Unit, Output_Proc : Ada_Qualified_Name;
      --  Qualified names for the unit that contains the buffer output
      --  procedure, and for the procedure itself.

      Dump_Trigger : Auto_Dump_Trigger := IC.Dump_Config.Trigger;
      --  Shortcut to avoid repeatedly restricting the dump trigger to the
      --  Auto_Dump_Trigger subtype.

   --  Start of processing for Emit_Dump_Helper_Unit

   begin

      if Language /= Ada_Language
         and then Dump_Trigger = Ravenscar_Task_Termination
      then
         Warn ("--dump-trigger=ravenscar-task-termination is not valid for a"
               & " C main. Defaulting to --dump-trigger=main-end for this"
               & " main.");
         Dump_Trigger := Main_End;
      end if;

      --  Create the name of the helper unit

      Helper_Unit := Sys_Buffers;
      Helper_Unit.Append
        (To_Unbounded_String ("D") & Instrumented_Unit_Slug (Main));

      --  Compute the qualified names we need for instrumentation

      declare
         use type Ada_Qualified_Name;
         Unit : constant String :=
           (case IC.Dump_Config.Channel is
               when Binary_File            => "Files",
               when Base64_Standard_Output => "Base64");
      begin
         Output_Unit := Sys_Prefix
           & To_Unbounded_String ("Traces")
           & To_Unbounded_String ("Output")
           & To_Unbounded_String (Unit);
         Output_Proc := Output_Unit & To_Unbounded_String ("Write_Trace_File");
      end;

      declare
         Helper_Unit_Name : constant String := To_Ada (Helper_Unit);
         Dump_Procedure   : constant String := To_String (Dump_Procedure_Name);

         Buffer_Units : constant Ada_Qualified_Name_Vectors.Vector :=
           Buffer_Units_For_Closure (IC, Main);
         --  List of names for units that contains the buffers to dump

      begin
         --  Emit the package spec. This includes one Dump_Buffers procedure,
         --  which dumps all coverage buffers in Main's closure to the source
         --  trace file.

         Create_File
           (Info,
            File,
            Name => To_Filename
                      (Info.Project,
                       CU_Name_For_Unit (Helper_Unit, Unit_Spec),
                       Ada_Language));

         Put_Warnings_And_Style_Checks_Pragmas (File);
         File.Put_Line ("package " & Helper_Unit_Name & " is");
         File.New_Line;
         File.Put_Line ("   procedure " & Dump_Procedure & ";");

         --  Export the symbol definition so that it can be used in other
         --  languages.

         File.Put_Line ("   pragma Export (C, " & Dump_Procedure & ", """
                        & Dump_Procedure_Symbol (Main) & """);");
         File.New_Line;

         case Dump_Trigger is
            when At_Exit | Ravenscar_Task_Termination =>
               File.Put_Line
                 ("procedure "
                  & To_String (Register_Dump_Procedure_Name) & ";");
               File.New_Line;

            when Main_End =>
               null;
         end case;

         File.Put_Line ("end " & Helper_Unit_Name & ";");
         File.Close;

         --  Emit the package body

         Create_File
           (Info,
            File,
            Name => To_Filename
                      (Info.Project,
                       CU_Name_For_Unit (Helper_Unit, Unit_Body),
                       Ada_Language));

         Put_Warnings_And_Style_Checks_Pragmas (File);

         Put_With (Output_Unit);
         for Buffer_Unit of Buffer_Units loop
            Put_With (Buffer_Unit);
         end loop;

         case Dump_Trigger is
            when At_Exit  =>
               File.Put_Line ("with Interfaces.C;");
            when Ravenscar_Task_Termination  =>
               File.Put_Line ("with Ada.Task_Identification;");
               File.Put_Line ("with Ada.Task_Termination;");
            when Main_End =>
               null;
         end case;

         File.Put_Line ("package body " & Helper_Unit_Name & " is");
         File.New_Line;

         --  Emit the procedure to write the trace file

         File.Put_Line ("   procedure " & Dump_Procedure & " is");
         File.Put_Line ("   begin");
         File.Put_Line ("      " & To_Ada (Output_Proc));
         File.Put      ("        ((");
         for Cur in Buffer_Units.Iterate loop
            declare
               use Ada_Qualified_Name_Vectors;

               Index       : constant Positive := To_Index (Cur);
               Buffer_Name : constant String :=
                 To_Ada (Element (Cur)) & ".Buffers";

            begin
               File.Put (Strings.Img (To_Index (Cur))
                         & " => " & Buffer_Name & "'Access");
               if Index = Buffer_Units.Last_Index then
                  File.Put_Line ("),");
               else
                  File.Put_Line (",");
                  File.Put ((1 .. 10 => ' '));
               end if;
            end;
         end loop;

         case IC.Dump_Config.Channel is
         when Binary_File =>
            declare
               use GNATCOLL.VFS;

               U       : constant String := To_Ada (Output_Unit);
               Indent1 : constant String := "         ";
               Indent2 : constant String := Indent1 & "  ";

               Env_Var : constant String :=
                 (if Length (IC.Dump_Config.Filename_Env_Var) = 0
                  then U & ".Default_Trace_Filename_Env_Var"
                  else """" & To_String (IC.Dump_Config.Filename_Env_Var)
                  & """");
               Prefix  : constant String :=
                 (if Length (IC.Dump_Config.Filename_Prefix) = 0
                  then """" & String'(+Info.Project.Executable_Name
                    (File => +To_Filename
                         (Project => Info.Project,
                          CU_Name => Main,
                          Language =>
                            (case Main.Language_Kind is
                                when Unit_Based_Language => Ada_Language,
                                when File_Based_Language => C_Language)),
                     Include_Suffix => True)) & """"
                  else """" & To_String (IC.Dump_Config.Filename_Prefix)
                  & """");
               Tag     : constant String := """" & To_String (IC.Tag) & """";
               Simple  : constant String :=
                 (if IC.Dump_Config.Filename_Simple
                  then "True"
                  else "False");
            begin
               File.Put_Line
                 (Indent1 & "Filename => " & U & ".Default_Trace_Filename");
               File.Put_Line (Indent2 & "(Prefix => " & Prefix & ",");
               File.Put_Line (Indent2 & " Env_Var => " & Env_Var & ",");
               File.Put_Line (Indent2 & " Tag => " & Tag & ",");
               File.Put (Indent2 & " Simple => " & Simple & ")");

               if Language = C_Language then

                  --  Same rationale as above

                  File.Put (",");
                  File.Put_Line
                    (Indent2 & "Program_Name => """
                     & (To_Filename (Info.Project, Main, Language))
                     & """");
               end if;
            end;

         when Base64_Standard_Output =>

            --  Configurations using this channel generally run on embedded
            --  targets and have a small runtime, so our best guess for the
            --  program name is the name of the main, and there is no way to
            --  get the current execution time.

            case Main.Language_Kind is
               when Unit_Based_Language =>
                  File.Put_Line
                    ("         Program_Name => """
                     & To_Ada (Main.Unit) & """,");
               when File_Based_Language =>
                  File.Put_Line
                    ("         Program_Name => """ & (+Main.Filename) & """,");
            end case;
            File.Put ("         Exec_Date => (others => ASCII.NUL)");
         end case;
         File.Put_Line (");");

         File.Put_Line ("   end " & Dump_Procedure & ";");
         File.New_Line;

         --  Emit trigger-specific procedures

         case Dump_Trigger is
            when At_Exit =>

               --  Emit a procedure to schedule a trace dump with atexit

               File.Put_Line
                 ("procedure "
                  & To_String (Register_Dump_Procedure_Name) & " is");
               File.Put_Line ("   type Callback is access procedure;");
               File.Put_Line ("   pragma Convention (C, Callback);");
               File.New_Line;
               File.Put_Line ("   function atexit (Func : Callback)"
                              & " return Interfaces.C.int;");
               File.Put_Line ("   pragma Import (C, atexit);");
               File.Put_Line ("   Dummy : constant Interfaces.C.int :=");
               File.Put_Line ("     atexit (" & Dump_Procedure & "'Access);");
               File.Put_Line ("begin");
               File.Put_Line ("   null;");
               File.Put_Line
                 ("end " & To_String (Register_Dump_Procedure_Name) & ";");
               File.New_Line;

            when Ravenscar_Task_Termination =>

               --  Emit a protected object for the callback

               File.Put_Line ("  protected Wrapper is");
               File.Put_Line ("     procedure Do_Dump"
                              & " (T : Ada.Task_Identification.Task_Id);");
               File.Put_Line ("  end Wrapper;");
               File.New_Line;
               File.Put_Line ("  protected body Wrapper is");
               File.Put_Line ("     procedure Do_Dump"
                              & " (T : Ada.Task_Identification.Task_Id) is");
               File.Put_Line ("        pragma Unreferenced (T);");
               File.Put_Line ("     begin");
               File.Put_Line ("        " & Dump_Procedure & ";");
               File.Put_Line ("     end Do_Dump;");
               File.Put_Line ("  end Wrapper;");
               File.New_Line;

               --  Emit a procedure to schedule a trace dump with
               --  Ada.Task_Termination.

               File.Put_Line
                 ("procedure "
                  & To_String (Register_Dump_Procedure_Name) & " is");
               File.Put_Line ("begin");
               File.Put_Line ("   Ada.Task_Termination"
                              & ".Set_Dependents_Fallback_Handler"
                              & " (Wrapper.Do_Dump'Access);");
               File.Put_Line
                 ("end " & To_String (Register_Dump_Procedure_Name) & ";");
               File.New_Line;

            when Main_End =>
               null;
         end case;

         File.Put_Line ("end " & Helper_Unit_Name & ";");
         File.Close;
      end;
   end Emit_Ada_Dump_Helper_Unit;

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
