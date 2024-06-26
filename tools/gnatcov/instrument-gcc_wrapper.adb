------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Containers;  use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Strings;     use Ada.Strings;
with Ada.Text_IO;     use Ada.Text_IO;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GNAT.OS_Lib;

with Coverage.Tags;           use Coverage.Tags;
with Files_Handling;          use Files_Handling;
with Files_Table;             use Files_Table;
with Instrument.C;            use Instrument.C;
with Instrument.Common;       use Instrument.Common;
with Instrument.Setup_Config; use Instrument.Setup_Config;
with Instrument.Source;
with Outputs;
with Paths;                   use Paths;
with Strings;                 use Strings;
with Subprocesses;            use Subprocesses;
with Switches;                use Switches;
with Temp_Dirs;               use Temp_Dirs;
with Traces_Files;
with Traces_Source;           use Traces_Source;

--  Implementation for the "gcc-wrapper" command

procedure Instrument.Gcc_Wrapper
  (Config_File   : String;
   Compiler_Exec : String;
   Cargs         : String_Vectors.Vector)
is
   Compiler_Exec_Basename : constant Unbounded_String :=
     +Simple_Name (Compiler_Exec);

   type Compilation_Command_Type is record
      Language : Any_Language;
      --  Language of the file that is compiled

      File : Virtual_File;
      --  File that is compiled

      Target : Virtual_File;
      --  Output assembly file (passed through the -o switch)

      Instrumentation_Sources : String_Vectors.Vector;
      --  List of sources produced by the instrumentation process. It does not
      --  include the instrumented version of the source.

   end record;
   --  Information relative to a compilation command launched by the compiler
   --  driver.

   No_Compilation_Command : constant Compilation_Command_Type :=
     (Language                => All_Languages,
      File                    => No_File,
      Target                  => No_File,
      Instrumentation_Sources => String_Vectors.Empty_Vector);

   package Compilation_Command_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Compilation_Command_Type);

   type Assembly_Command_Type is record
      Filename : Virtual_File;
      --  Assembly file (positional argument)

      Target : Virtual_File;
      --  Output object file (passed through the -o switch)

   end record;
   --  Information relative to an assembly command launched by the compiler
   --  driver.

   No_Assembly_Command : constant Assembly_Command_Type :=
     (Filename => No_File, Target => No_File);

   package Assembly_Command_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Assembly_Command_Type);

   type Link_Command_Type is record
      Target : Virtual_File;
      --  Output executable file (passed through the -o switch)

   end record;
   --  Information relative to a link command launched by the compiler driver

   No_Link_Command : constant Link_Command_Type := (Target => No_File);

   type Compilation_Database is record
      Compilation_Commands : Compilation_Command_Vectors.Vector;
      --  List of compilation commands launched by this compiler driver
      --  invocation.

      Assembly_Commands : Assembly_Command_Vectors.Vector;
      --  List of assembly commands launched by this compiler driver invocation

      Link_Command : Link_Command_Type := No_Link_Command;
      --  List of link commands launched by this compiler driver invocation

   end record;
   --  Track all of the commands launched by a compiler driver invocation

   type Parsing_Context is record
      Orig_Compiler_Driver : Unbounded_String;
      --  Full path to the original compiler driver

      Source_Mapping : File_To_File_Maps.Map;
      --  We rely on object file symbols to know what coverage buffers we
      --  should dump at link time. Nevertheless, an object file referenced in
      --  a link command (which we get through the -### verbose switch) does
      --  not necessarily exist yet: it can be a temporary file created by a
      --  previous compilation command that belongs to the same compiler driver
      --  invocation (e.g. when compiling and linking at the same time).
      --
      --  To support this case, we thus need to keep track of the temporary
      --  files created along the compilation process, to know to which source
      --  they ultimately refer, and derive the coverage buffer symbol from it.
      --
      --  The Source_Mapping thus maps the temporary object files to the
      --  original source.

      Instrumentation_Objects : File_To_String_Vectors_Maps.Map;
      --  Maps the original source name to the instrumentation artifact objects
      --  (e.g. coverage buffers unit, dump helper unit).

   end record;

   function Starts_With
     (Str : Unbounded_String; Pattern : String) return Boolean
   is (Index (Str, Pattern, 1) = 1);
   --  Returns whether the given Str starts with the given Pattern

   function Ends_With
     (Str : Unbounded_String; Pattern : String) return Boolean
   is (Length (Str) >= Pattern'Length
       and then
       Index
         (Str, Pattern, From => Positive (Length (Str)), Going => Backward)
       =
         Length (Str) - Pattern'Length + 1);
   --  Returns whether the given Str ends with the given Pattern

   function Split_Args (Command : String) return String_Vectors.Vector;

   function Img (Command : String_Vectors.Vector) return String;

   function Parse_Compiler_Driver_Command
     (Context : in out Parsing_Context;
      Prj     : in out Prj_Desc;
      Tmp_Dir : Temporary_Directory;
      Args    : String_Vectors.Vector) return Compilation_Database;
   --  Parse a compiler driver command

   function Parse_Compilation_Command
     (Context : in out Parsing_Context;
      Prj     : in out Prj_Desc;
      Command : String_Vectors.Vector) return Compilation_Command_Type;
   --  Parse a compilation command

   function Parse_Assembly_Command
     (Context : in out Parsing_Context;
      Command : String_Vectors.Vector) return Assembly_Command_Type;
   --  Parse an assembly command

   function Parse_Link_Command
     (Command : String_Vectors.Vector) return Link_Command_Type;
   --  Parse a link command

   function Coverage_Buffer_Symbols
     (Symbol_File     : Virtual_File;
      Tmp_Dir         : String;
      Compiler_Driver : String;
      Config          : Instrumentation_Config) return String_Sets.Set;
   --  Return the list of coverage buffer symbols in the given symbol file
   --  (object or library file).

   procedure Run_Original_Compiler
     (Context : Parsing_Context;
      Args    : String_Vectors.Vector);
   --  Run the wrapped compiler with the given Args

   ----------------
   -- Split_Args --
   ----------------

   function Split_Args (Command : String) return String_Vectors.Vector is
      type State_Kind is
        (No_Argument, Simple_Argument, Quoted_Argument);

      State  : State_Kind := No_Argument;
      Arg    : Unbounded_String;
      Result : String_Vectors.Vector;

      procedure Append_Arg;

      ----------------
      -- Append_Arg --
      ----------------

      procedure Append_Arg is
      begin
         if State /= No_Argument then
            Result.Append (Arg);
            State := No_Argument;
            Arg := Null_Unbounded_String;
         end if;
      end Append_Arg;

      C : Character;
      I : Natural := Command'First;

   begin
      while I <= Command'Last loop
         C := Command (I);
         case State is
            when No_Argument =>
               if C = '"' then
                  State := Quoted_Argument;
               elsif C /= ' ' then
                  State := Simple_Argument;
                  Append (Arg, C);
               end if;

            when Simple_Argument =>
               if C = ' ' then
                  Append_Arg;
               else
                  Append (Arg, C);
               end if;

            when Quoted_Argument =>
               if C = '\' then
                  I := I + 1;
                  Append (Arg, Command (I));
               elsif C = '"' then
                  Append_Arg;
               else
                  Append (Arg, C);
               end if;
         end case;
         I := I + 1;
      end loop;
      Append_Arg;
      return Result;
   end Split_Args;

   ---------
   -- Img --
   ---------

   function Img (Command : String_Vectors.Vector) return String is
      Result : Unbounded_String;
   begin
      for Arg of Command loop
         Append (Result, Arg);
         Append (Result, " ");
      end loop;
      return +Result;
   end Img;

   -----------------------------------
   -- Parse_Compiler_Driver_Command --
   -----------------------------------

   function Parse_Compiler_Driver_Command
     (Context : in out Parsing_Context;
      Prj     : in out Prj_Desc;
      Tmp_Dir : Temporary_Directory;
      Args    : String_Vectors.Vector) return Compilation_Database
   is
      use type String_Vectors.Vector;
      Result              : Compilation_Database;
      Parsed_Link_Command : Boolean := False;
      Commands_Filename   : constant String :=
        Tmp_Dir.Directory_Name / "commands";
   begin
      --  Expand the command line using gcc's -### option. TODO??? Check if the
      --  command we are intercepting is a compile / link target and not a
      --  preprocessing / -### action.

      Run_Command
        (+Context.Orig_Compiler_Driver,
         String_Vectors.To_Vector (+"-###", 1) & Args,
         "gnatcov",
         Output_File => Commands_Filename);

      --  Then, parse the files containing the list of launched commands, using
      --  the following heuristics:
      --
      --    * If the command is a cc1 invocation (first argument end with cc1),
      --      assume compilation command.
      --    * If the command is an as invocation, assume assembly command.
      --    * If the command is a collect2 invocation, assume link command.

      declare
         Commands_File : File_Type;
      begin
         Open (Commands_File, In_File, Commands_Filename);
         while not End_Of_File (Commands_File) loop
            declare
               Line    : constant String := Get_Line (Commands_File);
               Command : constant String_Vectors.Vector := Split_Args (Line);
            begin
               if Line = "" then
                  goto Continue;
               end if;
               if Ends_With (Command.First_Element, "cc1")
                 or else Ends_With (Command.First_Element, "cc1plus")
               then
                  declare
                     CC_Command : constant Compilation_Command_Type :=
                       Parse_Compilation_Command (Context, Prj, Command);
                  begin
                     if CC_Command /= No_Compilation_Command then
                        Result.Compilation_Commands.Append (CC_Command);
                     end if;
                  end;
               elsif Ends_With (Command.First_Element, "as") then
                  declare
                     As_Command : constant Assembly_Command_Type :=
                       Parse_Assembly_Command (Context, Command);
                  begin
                     if As_Command /= No_Assembly_Command then
                        Result.Assembly_Commands.Append (As_Command);
                     end if;
                  end;
               elsif Ends_With (Command.First_Element, "collect2") then

                  --  Assume that we can only have a single link command. If
                  --  that's not the case, error out.

                  if Parsed_Link_Command then
                     Outputs.Fatal_Error
                       ("The compiler driver invocation yielded several link"
                        & " commands, which is not supported");
                  end if;
                  Result.Link_Command :=
                    (Parse_Link_Command (Command));
                  Parsed_Link_Command := True;
               end if;
               <<Continue>>
            end;
         end loop;
      end;
      return Result;
   end Parse_Compiler_Driver_Command;

   -------------------------------
   -- Parse_Compilation_Command --
   -------------------------------

   function Parse_Compilation_Command
     (Context : in out Parsing_Context;
      Prj     : in out Prj_Desc;
      Command : String_Vectors.Vector) return Compilation_Command_Type
   is
      use String_Vectors;
      PP_Args : String_Vectors.Vector;
      --  List of arguments that should be passed to the preprocessor
      --  invocation: basically all of the arguments except the compiled source
      --  and the -o switch.

      Result : Compilation_Command_Type;
      Cur    : Cursor := First (Command);
   begin
      if Ends_With (Command.First_Element, "cc1plus") then
         Result.Language := CPP_Language;
      elsif Ends_With (Command.First_Element, "cc1") then
         Result.Language := C_Language;
      end if;

      --  Skip the first argument as it is the compiler executable, and not
      --  a compiler argument.

      Cur := Next (Cur);

      while Has_Element (Cur) loop
         declare
            Arg : constant Unbounded_String :=
              String_Vectors.Element (Cur);
         begin
            --  Skip switches arguments that look like filenames. Ideally, we
            --  would find the positional argument but it is not
            --  straightforward.

            if +Arg in "-dumpbase" | "-dumpbase-ext" then
               Cur := Next (Cur);

            --  TODO??? The user can configure the file extension and the
            --  implementation should be resilient to this.

            elsif Ends_With (Arg, ".c")
              or else Ends_With (Arg, ".cc")
              or else Ends_With (Arg, ".cpp")
              or else Ends_With (Arg, ".cxx")
            then
               if Result.File = No_File then
                  Result.File := Create_Normalized (+Arg);
               else
                  Outputs.Warn
                    ("Found multiple filenames in the compiler invocation: "
                     & (+Result.File.Base_Name) & " and " & (+Arg)
                     & ". Keeping the former, which was parsed before.");
               end if;
            elsif Arg = "-o" then
               Cur := Next (Cur);
               Result.Target :=
                 Create_Normalized (+String_Vectors.Element (Cur));
            else
               PP_Args.Append (Arg);
            end if;
         end;
         Cur := Next (Cur);
      end loop;

      if Result.File = No_File or else Result.Target = No_File then
         return No_Compilation_Command;
      end if;
      Prj.Compiler_Driver (Result.Language) := Command.First_Element;
      Prj.Compiler_Options_Unit.Insert (Result.File, PP_Args);
      Context.Source_Mapping.Include (Result.Target, Result.File);
      return Result;
   end Parse_Compilation_Command;

   ----------------------------
   -- Parse_Assembly_Command --
   ----------------------------

   function Parse_Assembly_Command
     (Context : in out Parsing_Context;
      Command : String_Vectors.Vector) return Assembly_Command_Type
   is
      Result : Assembly_Command_Type;
   begin
      for Cur in Command.Iterate loop
         declare
            Arg : constant Unbounded_String :=
              String_Vectors.Element (Cur);
         begin
            if Arg = "-o" then
               Result.Target :=
                 Create_Normalized
                   (+String_Vectors.Element (String_Vectors.Next (Cur)));
            elsif Ends_With (Arg, ".s") then
               Result.Filename := Create_Normalized (+Arg);
            end if;
         end;
      end loop;

      --  Error out if the parsing failed

      if Result.Filename = No_File then
         Outputs.Fatal_Error
           ("Could not find assembly file in assembly command: "
            & Img (Command));
      elsif Result.Target = No_File then
         Outputs.Fatal_Error
           ("Could not find output file in assembly command: "
            & Img (Command));
      end if;

      --  If we do not find the original source in the mapping, assume that
      --  this is not a file of interest.

      if not Context.Source_Mapping.Contains (Result.Filename) then
         return No_Assembly_Command;
      end if;

      Context.Source_Mapping.Insert
        (Result.Target, Context.Source_Mapping.Element (Result.Filename));
      return Result;
   end Parse_Assembly_Command;

   ------------------------
   -- Parse_Link_Command --
   ------------------------

   function Parse_Link_Command
     (Command : String_Vectors.Vector) return Link_Command_Type
   is
      use String_Vectors;
      Result : Link_Command_Type;
   begin
      --  Find the libraries and library directories in the link command

      for Cur in Command.Iterate loop
         declare
            Arg : constant Unbounded_String := Element (Cur);
         begin
            if Arg = "-o" then
               Result.Target := Create_Normalized (+Element (Next (Cur)));
            end if;
         end;
      end loop;

      if Result.Target = No_File then
         Result.Target := Create_Normalized ("a.out");
      end if;

      --  We are only interested in link commands yielding an executable,
      --  ignore all others. Use the output name as an heuristic.

      declare
         Target : constant String := +Result.Target.Base_Name;
      begin
         if Ends_With (+Target, ".so")
           or else Ends_With (+Target, ".o")
         then
            return No_Link_Command;
         end if;
      end;

      return Result;
   end Parse_Link_Command;

   -----------------------------
   -- Coverage_Buffer_Symbols --
   -----------------------------

   function Coverage_Buffer_Symbols
     (Symbol_File     : Virtual_File;
      Tmp_Dir         : String;
      Compiler_Driver : String;
      Config          : Instrumentation_Config) return String_Sets.Set
   is
      Args            : String_Vectors.Vector;
      Output_Filename : constant String :=
        Tmp_Dir / ("nm_" & Filename_Slug (+Symbol_File.Full_Name));
      Output_File     : File_Type;

      Result : String_Sets.Set;

   begin
      --  Use the compiler nm to dump the list of symbols

      Args.Append (+"--format=just-symbol");
      Args.Append (Full_Name (Symbol_File));
      Run_Command
        (Command             =>
           +Config.Nms.Element (+Base_Name (Compiler_Driver)),
         Arguments           => Args,
         Origin_Command_Name => "compiler wrapper",
         Output_File         => Output_Filename);

      --  Each line of the output is a symbol

      Open (Output_File, In_File, Output_Filename);

      while not End_Of_File (Output_File) loop
         declare
            Line : constant Unbounded_String := +Get_Line (Output_File);
         begin
            if Starts_With (Line, "gnatcov_rts_buffers")

              --  The buffer list symbol is also referenced in the link
              --  closure: make sure not to pick it as it is named
              --  gnatcov_rts_buffers_array_<prj_name>, so
              --  gnatcov_rts_buffers_array_main in our case.

              and then Ends_With (Line, "_buffers")
            then
               Result.Insert (Line);
            end if;
         end;
      end loop;

      if not Save_Temps then
         Delete (Output_File);
      else
         Close (Output_File);
      end if;
      return Result;
   end Coverage_Buffer_Symbols;

   ---------------------------
   -- Run_Original_Compiler --
   ---------------------------

   procedure Run_Original_Compiler
     (Context : Parsing_Context;
      Args    : String_Vectors.Vector) is
   begin
      Run_Command
        (Command             => +Context.Orig_Compiler_Driver,
         Arguments           => Args,
         Origin_Command_Name => "compiler");
   end Run_Original_Compiler;

   Compiler_Wrapper_Dir : constant String :=
     Containing_Directory (Config_File);
   --  Directory that contains the current program

   Instr_Config : Instrumentation_Config :=
     Load_Config (Config_File);
   --  Instrumentation configuration previously generated by the setup step

   Instr_Dir : Temporary_Directory;
   --  Directory holding instrumentation artefacts

   Comp_DB : Compilation_Database;

   Prj : Prj_Desc;
   --  Artificial project description to pass to the various instrumentation
   --  workflows.

   Buffers_List_Unit : Compilation_Unit;
   --  Name of the unit holding the buffer list definitions, if this compiler
   --  driver invocation expands to a link command.

   Context : Parsing_Context;

   Instrumented_Files : String_Sets.Set;
   --  List of instrumented files (files of interest / main files / both)

   Files_Of_Interest : File_Sets.Set;
   --  List of files of interest

--  Start of processing for Compiler_Wrappers.GCC

begin
   Create_Temporary_Directory
     (Instr_Dir, "gnatcov_instr", Auto_Delete => not Switches.Save_Temps);

   --  Set things that must be set as we don't go through gnatcov_bits_specific

   Tag_Provider := Tag_Providers.Create (Default_Tag_Provider_Name);
   Traces_Files.Update_Current_Trace_Kind (Traces_Files.Source_Trace_File);

   Context.Orig_Compiler_Driver :=
     Instr_Config.Compiler_Drivers.Element (Compiler_Exec_Basename);

   --  If this driver invocation is not meant to compile a source file, there
   --  is no instrumentation to do: just run the original driver and exit.

   for Arg of Cargs loop
      if +Arg in "-###" | "-E" then
         Run_Original_Compiler (Context, Cargs);
         return;
      end if;
   end loop;

   --  Get the files of interest

   for C in Instr_Config.File_To_SID.Iterate loop
      Files_Of_Interest.Insert (File_To_String_Maps.Key (C));
   end loop;

   --  Parse the compiler driver invocation

   Comp_DB := Parse_Compiler_Driver_Command (Context, Prj, Instr_Dir, Cargs);

   --  Generate an artificial project description to pass compiler switches and
   --  default spec / body suffixes.

   Prj.Prj_Name := To_Qualified_Name ("main");
   Prj.Output_Dir := +Instr_Dir.Directory_Name;
   Prj.Spec_Suffix :=
     (C_Language => +".h", CPP_Language => +".hh", others => <>);
   Prj.Body_Suffix :=
     (C_Language => +".c", CPP_Language => +".cc", others => <>);

   --  Then, invoke the right set of gnatcov commands

   --  Start by the compilation command: for each compilation command, we
   --  must instrument its source and substitute on the command line the
   --  original file with its instrumented version.

   for Cur in Comp_DB.Compilation_Commands.Iterate loop
      declare
         use Compilation_Command_Vectors;
         Comp_Command     : constant Compilation_Command_Type := Element (Cur);
         Comp_Command_Ref : constant Reference_Type :=
           Comp_DB.Compilation_Commands.Reference (Cur);
         Instrumenter     : Language_Instrumenter'Class :=
           (case Comp_Command.Language is
           when C_Language   =>
             Create_C_Instrumenter
                (Instr_Config.Tag, Integrated_Instrumentation),
           when CPP_Language =>
             Create_CPP_Instrumenter
                (Instr_Config.Tag, Integrated_Instrumentation),
           when others       =>
              raise Program_Error
                with "Unsupported language for integrated instrumentation");

         Fullname    : constant String := +Comp_Command.File.Full_Name;
         Simple_Name : constant String := +Comp_Command.File.Base_Name;
         Instr_Name  : constant String := (+Prj.Output_Dir) / Simple_Name;

         FI : constant File_To_String_Maps.Cursor :=
           Instr_Config.File_To_SID.Find (Comp_Command.File);
      begin
         --  Start by instrumenting the file as a source, if it is a unit of
         --  interest.

         if File_To_String_Maps.Has_Element (FI) then

            --  Pass the compiler switches through the project description

            Instrument.Source
              (Unit_Name         => Fullname,
               SID_Name          =>
                 Compiler_Wrapper_Dir /
                   (+File_To_String_Maps.Element (FI)),
               Instrumenter      => Instrumenter,
               Files_Of_Interest => Files_Of_Interest,
               Prj               => Prj);

            Comp_Command_Ref.Instrumentation_Sources.Append
              (Instrumenter.Buffer_Unit
                 (Compilation_Unit'
                    (File_Based_Language, Full_Name (Comp_Command.File)),
                  Prj)
               .Unit_Name);
            Instrumented_Files.Include (+Fullname);
         end if;

         --  Then, instrument it as a main if it is one

         if Instrumenter.Has_Main (Fullname, Prj) then

            --  Pick as the trace name prefix the base name of the main
            --  filename

            if Instr_Config.Dump_Config.Channel = Binary_File then
               Instr_Config.Dump_Config.Filename_Prefix :=
                 +Ada.Directories.Base_Name (Fullname);
            end if;

            Instrumenter.Auto_Dump_Buffers_In_Main
              (Instr_Name, Instr_Config.Dump_Config, Prj);
            Comp_Command_Ref.Instrumentation_Sources.Append
              (Instrumenter.Dump_Helper_Unit
                 (Compilation_Unit'
                      (File_Based_Language, +Instr_Name),
                  Prj)
               .Unit_Name);
            Instrumented_Files.Include (+Fullname);
         end if;
      end;
   end loop;

   --  If this is a link command, add an empty buffers list unit to the link
   --  closure. We will compute the correct buffers list unit at the second
   --  link.

   if Comp_DB.Link_Command /= No_Link_Command then
      declare
         Instrumenter : constant Language_Instrumenter'Class :=
           Create_C_Instrumenter
             (Instr_Config.Tag, Integrated_Instrumentation);
         --  Emit the buffers list unit as a C compilation unit as it is
         --  compilable by a C / C++ compiler, which are the languages
         --  supported by the integrated instrumentation scheme.

      begin
         Buffers_List_Unit :=
           Instrumenter.Emit_Buffers_List_Unit (String_Sets.Empty_Set, Prj);
      end;
   end if;

   --  Now that we have all of the instrumentation artifacts, launch the
   --  original compiler driver command.

   declare
      Output_Dir : constant String := +Prj.Output_Dir;
      New_Args   : String_Vectors.Vector := Cargs;
   begin
      New_Args.Prepend ("-I" & Instr_Config.GNATcov_RTS_Include_Dir);

      if Comp_DB.Link_Command /= No_Link_Command then
         New_Args.Append (+"-lgnatcov_rts");
         New_Args.Prepend ("-L" & Instr_Config.GNATcov_RTS_Object_Dir);
      end if;

      --  Start with adding the buffer list unit (if it was emitted) to the
      --  compilation closure.

      if Length (Buffers_List_Unit.Unit_Name) /= 0 then
         New_Args.Prepend (Buffers_List_Unit.Unit_Name);
      end if;

      --  Then, substitute files of interest with their instrumented version,
      --  which were generated in Prj.Output_Dir.

      for I in 0 .. Natural (String_Vectors.Length (New_Args) - 1) loop
         declare
            Arg : constant Unbounded_String := New_Args.Element (I);
         begin
            if Ada.Directories.Exists (+Arg) then
               declare
                  Base     : constant String := Simple_Name (+Arg);
                  Fullname : constant String := Full_Name (+Arg);
               begin
                  if Instrumented_Files.Contains (+Fullname) then
                     New_Args.Replace_Element (I, +(Output_Dir / Base));
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Deal with the instrumentation artifacts

      for Cur in Comp_DB.Compilation_Commands.Iterate loop
         declare
            use Compilation_Command_Vectors;
            Comp_Command : constant Compilation_Command_Type := Element (Cur);
         begin
            --  If this is a link command, add the instrumentation artifacts
            --  to the compiler driver command

            if Comp_DB.Link_Command /= No_Link_Command then
               for Instr_Artifact of Comp_Command.Instrumentation_Sources loop
                  New_Args.Prepend (Instr_Artifact);
               end loop;

            --  Otherwise, compile the instrumentation artifacts and package
            --  them later with the instrumented source object file once we
            --  assemble it.

            else
               Context.Instrumentation_Objects.Insert
                 (Comp_Command.File, String_Vectors.Empty);

               for Instr_Artifact of Comp_Command.Instrumentation_Sources loop
                  declare
                     Args_Compilation : String_Vectors.Vector;
                     Instr_Artifact_Object_Name : constant String :=
                       New_File
                         (Prj,
                          Ada.Directories.Base_Name (+Instr_Artifact) & ".o");
                  begin
                     Args_Compilation.Append (+"-c");
                     Args_Compilation.Append
                       ("-I" & Instr_Config.GNATcov_RTS_Include_Dir);
                     Args_Compilation.Append (Instr_Artifact);
                     Args_Compilation.Append (+"-o");
                     Args_Compilation.Append (+Instr_Artifact_Object_Name);
                     Run_Original_Compiler (Context, Args_Compilation);

                     Context.Instrumentation_Objects
                       .Reference (Comp_Command.File)
                       .Append (+Instr_Artifact_Object_Name);
                  end;
               end loop;
            end if;
         end;
      end loop;

      --  Finally, run the alternate compiler driver invocation

      Run_Original_Compiler (Context, New_Args);

      if Comp_DB.Link_Command = No_Link_Command then

         --  Merge coverage buffer object files with instrumented files for
         --  each assembly command.

         for Assembly_Command of Comp_DB.Assembly_Commands loop
            if Context.Source_Mapping.Contains (Assembly_Command.Filename) then
               declare
                  Orig_Source   : constant Virtual_File :=
                    Context.Source_Mapping.Element (Assembly_Command.Filename);
                  Instr_Objects : constant String_Vectors.Vector :=
                    Context.Instrumentation_Objects.Element (Orig_Source);
                  Packaged_Name : constant String :=
                    New_File
                      (Prj,
                       "instr_" & Filename_Slug (+Orig_Source.Full_Name)
                       & ".a");
                  Success : Boolean;
               begin
                  if not Instr_Objects.Is_Empty then
                     declare
                        Args_Ld : String_Vectors.Vector;
                     begin
                        Args_Ld.Append (+"-r");
                        Args_Ld.Append_Vector (Instr_Objects);
                        Args_Ld.Append (Full_Name (Assembly_Command.Target));
                        Args_Ld.Append (+"-o");
                        Args_Ld.Append (+Packaged_Name);
                        Run_Command
                          (Command             =>
                             +Instr_Config.Linkers.Element
                               (Compiler_Exec_Basename),
                           Arguments           => Args_Ld,
                           Origin_Command_Name => "compiler wrapper");

                        --  Finally, replace the original object file with the
                        --  newly created library file, packaging both the
                        --  instrumented source and its coverage buffer.

                        GNAT.OS_Lib.Copy_File
                          (Packaged_Name,
                           +Assembly_Command.Target.Full_Name,
                           Success,
                           Mode     => GNAT.OS_Lib.Overwrite,
                           Preserve => GNAT.OS_Lib.Full);
                     end;
                  end if;
               end;
            end if;
         end loop;
      end if;

      --  If this was a link command, the first link is done with a dummy
      --  buffers unit list, to know precisely the list of buffer symbols
      --  included in the executable. We generate an accurate buffers unit list
      --  by inspecting the executable symbols, and link a second time with
      --  this buffers unit list.
      --
      --  Note that the buffers list unit compilation unit is included in the
      --  compiler driver invocation (through New_Args), and the buffers
      --  list compilation unit generated below overwrites the dummy one.

      if Comp_DB.Link_Command /= No_Link_Command
         and then Ada.Directories.Exists
           (+Comp_DB.Link_Command.Target.Full_Name)
      then
         declare
            Instrumenter : constant Language_Instrumenter'Class :=
              Create_C_Instrumenter
                (Instr_Config.Tag, Integrated_Instrumentation);
            --  Emit the buffers list unit as a C compilation unit as it is
            --  compilable by a C / C++ compiler, which are the languages
            --  supported by the integrated instrumentation scheme.

         begin
            --  Generate the buffers list unit

            Buffers_List_Unit :=
              Instrumenter.Emit_Buffers_List_Unit
                (Coverage_Buffer_Symbols
                   (Symbol_File     => Comp_DB.Link_Command.Target,
                    Tmp_Dir         => +Prj.Output_Dir,
                    Compiler_Driver => Compiler_Exec,
                    Config          => Instr_Config),
                 Prj);

            --  Then, re-run the link with the correct buffers list unit

            Run_Original_Compiler (Context, New_Args);
         end;
      end if;
   end;

end Instrument.Gcc_Wrapper;
