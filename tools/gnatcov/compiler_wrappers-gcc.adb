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

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories;       use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNAT.OS_Lib;

with Coverage.Tags;           use Coverage.Tags;
with Files_Table;             use Files_Table;
with GNATcov_RTS.Buffers;     use GNATcov_RTS.Buffers;
with Instrument;              use Instrument;
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

procedure Compiler_Wrappers.Gcc
is
   type Compilation_Command_Type is record
      Language : Any_Language;
      --  Language of the file that is compiled

      Filename : Unbounded_String;
      --  Full name of the file that is compiled

      Target : Unbounded_String;
      --  Name of the output assembly file (passed through the -o switch)

      Instrumentation_Sources : String_Vectors.Vector;
      --  List of sources produced by the instrumentation process. It does not
      --  include the instrumented version of the source.

   end record;
   --  Information relative to a compilation command launched by the compiler
   --  driver.

   No_Compilation_Command : constant Compilation_Command_Type :=
     (Language                => All_Languages,
      Filename                => +"",
      Target                  => +"",
      Instrumentation_Sources => String_Vectors.Empty_Vector);

   package Compilation_Command_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Compilation_Command_Type);

   type Assembly_Command_Type is record
      Filename : Unbounded_String;
      --  Name of the assembly file (positional argument)

      Target : Unbounded_String;
      --  Output object file (passed through the -o switch)

   end record;
   --  Information relative to an assembly command launched by the compiler
   --  driver.

   No_Assembly_Command : constant Assembly_Command_Type :=
     (Filename => +"",
      Target   => +"");

   package Assembly_Command_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Assembly_Command_Type);

   type Link_Command_Type is record
      Library_Dirs  : String_Vectors.Vector;
      --  List of library directories arguments (passed through -L switches)

      Libraries : String_Vectors.Vector;
      --  List of libraries (passed through -l switches)

      Object_Files  : String_Vectors.Vector;
      --  List of object files (positional arguments)

      Source_Files  : String_Vectors.Vector;
      --  When the compiler driver command compiles and links, this contains
      --  the list of files that are compiled by the compiler driver command.

      Target : Unbounded_String;
      --  Output executable file (passed through the -o switch)

   end record;
   --  Information relative to a link command launched by the compiler driver

   No_Link_Command : constant Link_Command_Type :=
     (Library_Dirs => String_Vectors.Empty_Vector,
      Libraries    => String_Vectors.Empty_Vector,
      Object_Files => String_Vectors.Empty_Vector,
      Source_Files => String_Vectors.Empty_Vector,
      Target       => +"");

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

      Source_Mapping : String_Maps.Map;
      --  We rely on object file symbols to know what coverage buffers we
      --  should dump at link time. Nevertheless, an object file referenced in
      --  a link command (which we get through the -### verbose switch) does
      --  not necessarily exists yet: it can be a temporary file created by a
      --  previous compilation command that belongs to the same compiler driver
      --  invocation (e.g. when compiling and linking at the same time).
      --
      --  To support this case, we thus need to keep track of the temporary
      --  files created along the compilation process, to know to which source
      --  they ultimately refer, and derive the coverage buffer symbol from it.
      --
      --  The Source_Mapping thus maps the temporary object files to the
      --  original source.

      Instrumentation_Objects : String_Vectors_Maps.Map;
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
      Tmp_Dir : Temporary_Directory;
      Args    : String_Vectors.Vector) return Compilation_Database;
   --  Parse a compiler driver command

   function Parse_Compilation_Command
     (Context : in out Parsing_Context;
      Command : String_Vectors.Vector) return Compilation_Command_Type;
   --  Parse a compilation command

   function Parse_Assembly_Command
     (Context : in out Parsing_Context;
      Command : String_Vectors.Vector) return Assembly_Command_Type;
   --  Parse an assembly command

   function Parse_Link_Command
     (Context : Parsing_Context;
      Command : String_Vectors.Vector) return Link_Command_Type;
   --  Parse a link command

   function Coverage_Buffer_Symbols
     (Tmp_Dir         : String;
      Command         : Link_Command_Type;
      Compiler_Driver : String;
      Config          : Instrumentation_Config) return String_Sets.Set;
   --  Return the list of coverage buffer symbols in the link closure

   procedure Run_Original_Compiler
     (Context : Parsing_Context;
      Args    : String_Vectors.Vector);
   --  Run the wrapped compiler with the given Args

   ----------------
   -- Split_Args --
   ----------------

   function Split_Args (Command : String) return String_Vectors.Vector
   is
      Arg : Unbounded_String;
      Result : String_Vectors.Vector;
   begin
      for C of Command loop
         if C = ' ' and then Arg /= "" then
            Result.Append (Arg);
            Arg := +"";
         else
            Append (Arg, C);
         end if;
      end loop;
      if Arg /= "" then
         Result.Append (Arg);
      end if;
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
      Tmp_Dir : Temporary_Directory;
      Args    : String_Vectors.Vector) return Compilation_Database
   is
      use type String_Vectors.Vector;
      Result              : Compilation_Database;
      Parsed_Link_Command : Boolean := False;
      Commands_Filename   : constant String :=
        Tmp_Dir.Directory_Name / "commands";
   begin
      --  Expand the command line using gcc's -### option. TODO??? check if
      --  the command we are intercepting is a compile / link target and not
      --  a preprocessing / -### action.

      Run_Command
        (+Context.Orig_Compiler_Driver,
         String_Vectors.To_Vector (+"-###", 1) & Args,
         "gnatcov",
         Output_File => Commands_Filename);

      --  Then, parse the files containing the list of launched commands, using
      --  the following heuristics:
      --    * If the command is a cc1 invocation (first argument end with cc1),
      --      assume compilation command.
      --    * If the command is an as invocation, assume assembly command.
      --    * If the command is a collect2 invocation, assume link command

      declare
         Commands_File : File_Type;
      begin
         Open (Commands_File, In_File, Commands_Filename);
         while not End_Of_File (Commands_File) loop
            declare
               Line : constant String := Get_Line (Commands_File);
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
                       Parse_Compilation_Command (Context, Command);
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
                    (Parse_Link_Command (Context, Command));
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
      Command : String_Vectors.Vector) return Compilation_Command_Type
   is
      use String_Vectors;
      Result : Compilation_Command_Type;
      Cur    : Cursor := First (Command);
   begin
      if Ends_With (Command.First_Element, "cc1plus") then
         Result.Language := CPP_Language;
      elsif Ends_With (Command.First_Element, "cc1") then
         Result.Language := C_Language;
      end if;

      while Has_Element (Cur) loop
         declare
            Arg : constant Unbounded_String :=
              String_Vectors.Element (Cur);
         begin
            --  Skip switches arguments that look like filenames. Ideally,
            --  we would find the positional argument but it is not
            --  straightforward.

            if Arg = +"-dumpbase" or else Arg = +"-dumpbase-ext" then
               Cur := Next (Cur);
            end if;

            if Ends_With (Arg, ".c")
              or else Ends_With (Arg, ".cc")
              or else Ends_With (Arg, ".cpp")
              or else Ends_With (Arg, ".cxx")
            then
               if Length (Result.Filename) = 0 then
                  Result.Filename := +Ada.Directories.Full_Name (+Arg);
               else
                  Outputs.Warn
                    ("Found multiple filenames in the compiler invocation: "
                     & (+Result.Filename) & " and " & (+Arg)
                     & ". Keeping the former, which was parsed before.");
               end if;
            elsif Arg = "-o" then
               Result.Target :=
                 String_Vectors.Element (String_Vectors.Next (Cur));
            end if;
         end;
         Cur := Next (Cur);
      end loop;

      if Length (Result.Filename) = 0
         or else Length (Result.Target) = 0
      then
         return No_Compilation_Command;
      end if;
      Context.Source_Mapping.Include (Result.Target, Result.Filename);
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
                 String_Vectors.Element (String_Vectors.Next (Cur));
            elsif Ends_With (Arg, ".s") then
               Result.Filename := Arg;
            end if;
         end;
      end loop;

      --  Error out if the parsing failed

      if Length (Result.Filename) = 0 then
         Outputs.Fatal_Error
           ("Could not find assembly file in assembly command: "
            & Img (Command));
      elsif Length (Result.Target) = 0 then
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
     (Context : Parsing_Context;
      Command : String_Vectors.Vector) return Link_Command_Type
   is
      use String_Vectors;
      Result : Link_Command_Type;
   begin
      --  Find the libraries and library directories in the link command

      for Cur in Command.Iterate loop
         declare
            Arg : constant Unbounded_String := Element (Cur);
         begin
            if Arg = +"-L" then
               Result.Library_Dirs.Append (Element (Next (Cur)));
            elsif Starts_With (Arg, "-L") then
               Result.Library_Dirs.Append
                 (Unbounded_Slice (Arg, 3, Length (Arg)));
            elsif Arg = +"-l" then
               Result.Libraries.Append (Element (Next (Cur)));
            elsif Starts_With (Arg, "-l") then
               Result.Libraries.Append
                 (Unbounded_Slice (Arg, 3, Length (Arg)));
            elsif Arg = +"-o" then
               Result.Target := Element (Next (Cur));
            elsif Ends_With (Arg, ".o") then
               if Context.Source_Mapping.Contains (Arg) then
                  Result.Source_Files.Append
                    (Context.Source_Mapping.Element (Arg));
               else
                  Result.Object_Files.Append (Arg);
               end if;
            end if;
         end;
      end loop;

      if Length (Result.Target) = 0 then
         Result.Target := +"a.out";
      end if;

      return Result;
   end Parse_Link_Command;

   -----------------------------
   -- Coverage_Buffer_Symbols --
   -----------------------------

   function Coverage_Buffer_Symbols
     (Tmp_Dir         : String;
      Command         : Link_Command_Type;
      Compiler_Driver : String;
      Config          : Instrumentation_Config) return String_Sets.Set
   is
      function Coverage_Buffer_Symbols
        (Symbol_File : String) return String_Sets.Set;
      --  Return the list of coverage buffer symbols in the given symbol file
      --  (object or library file).

      -----------------------------
      -- Coverage_Buffer_Symbols --
      -----------------------------

      function Coverage_Buffer_Symbols
        (Symbol_File : String) return String_Sets.Set
      is
         Args            : String_Vectors.Vector;
         Output_Filename : constant String :=
           Tmp_Dir / ("nm_" & Filename_Slug (Symbol_File));
         Output_File     : File_Type;

         Result         : String_Sets.Set;
         Ignore_Success : Boolean;

      begin
         --  Use the compiler nm to dump the list of symbols

         Args.Append (+"--format=just-symbol");
         Args.Append (+Symbol_File);

         --  The command can fail with e.g. "file format not recognized" for
         --  system libraries. TODO???: investigate why. We should also avoid
         --  invoking nm on system libraries altogether.

         Ignore_Success :=
           Run_Command
             (Command             =>
                +Config.Nms.Element (+Base_Name (Compiler_Driver)),
              Arguments           => Args,
              Origin_Command_Name => "compiler wrapper",
              Output_File         => Output_Filename,
              Ignore_Error        => True);

         --  Each line of the output is a symbol

         Open (Output_File, In_File, Output_Filename);

         while not End_Of_File (Output_File) loop
            declare
               Line_Str : constant String := Get_Line (Output_File);
               Line : constant Unbounded_String := +Line_Str;
            begin
               if Starts_With (Line, "gnatcov_rts_buffers")

                 --  The buffer list symbols is also referenced in the link
                 --  closure: make sure not to pick it as it is named
                 --  gnatcov_rts_buffers_array_<prj_name>, so
                 --  gnatcov_rts_buffers_array_main in our case.

                 and then Ends_With (Line, "_buffers")
               then
                  Result.Insert (Line);
               end if;
            end;
         end loop;
         Close (Output_File);
         return Result;
      end Coverage_Buffer_Symbols;

      Result : String_Sets.Set;

   begin
      --  Search through object files and libraries the list of coverage buffer
      --  symbols.

      --  Start by dealing with object files

      for Object_File of Command.Object_Files loop
         Result.Union (Coverage_Buffer_Symbols (+Object_File));
      end loop;

      --  Then, deal with library files

      declare
         Library_Path : Unbounded_String;
      begin
         for Library_Dir of Command.Library_Dirs loop
            Append (Library_Path, Library_Dir);
            Append (Library_Path, GNAT.OS_Lib.Path_Separator);
         end loop;
         Append
           (Library_Path,
            Ada.Environment_Variables.Value ("LIBRARY_PATH", ""));

         for Library of Command.Libraries loop
            declare
               use type GNAT.OS_Lib.String_Access;
               Library_File : GNAT.OS_Lib.String_Access :=
                 GNAT.OS_Lib.Locate_Regular_File
                   ("lib" & (+Library) & ".a", +Library_Path);
            begin
               if Library_File /= null then
                  Result.Union (Coverage_Buffer_Symbols (Library_File.all));
                  GNAT.OS_Lib.Free (Library_File);
               end if;
            end;
         end loop;
      end;

      --  Deal with sources that were compiled _and_ linked by the same driver
      --  command.

      for Source of Command.Source_Files loop
         if Switches.Files_Of_Interest.Contains (Source) then
            declare
               Unit : constant Compilation_Unit :=
                 (Language  => File_Based_Language,
                  Unit_Name => Source);
            begin
               Result.Insert (+Unit_Buffers_Name (Unit));
            end;
         end if;
      end loop;

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

   Compiler_Wrapper_Dir : constant String := Containing_Directory
     (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all);
   --  Directory that contains the current program

   Instr_Config_File : constant String :=
     Compiler_Wrapper_Dir / Instrumentation_Config_Filename;
   Instr_Config : Instrumentation_Config :=
     Load_Config (Instr_Config_File);
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

   Compiler_Driver_Opts : String_Vectors.Vector;
   --  List of options passed to the compiler driver

   Instrumented_Files : String_Sets.Set;
   --  List of instrumented files (files of interest / main files / both)

begin
   Create_Temporary_Directory
     (Instr_Dir, "gnatcov_instr", Auto_Delete => not Switches.Save_Temps);

   --  Set things that must be set as we don't go through gnatcov_bits_specific

   Tag_Provider := Tag_Providers.Create (Default_Tag_Provider_Name);
   Traces_Files.Update_Current_Trace_Kind (Traces_Files.Source_Trace_File);

   Context.Orig_Compiler_Driver :=
     Instr_Config.Compiler_Drivers.Element
       (+Ada.Directories.Simple_Name (Command_Name));

   --  Load the command line

   for I in 1 .. Argument_Count loop
      Compiler_Driver_Opts.Append (+Argument (I));
   end loop;

   --  If this driver invocation is not meant to compile a source file, there
   --  is no instrumentation to do: just run the original driver and exit.

   for Arg of Compiler_Driver_Opts loop
      if +Arg in "-###" | "-E" then
         Run_Original_Compiler (Context, Compiler_Driver_Opts);
         return;
      end if;
   end loop;

   --  Parse the compiler driver invocation

   Comp_DB :=
     Parse_Compiler_Driver_Command (Context, Instr_Dir, Compiler_Driver_Opts);

   --  Generate an artificial project description to pass compiler
   --  switches and default spec / body suffixes.

   Prj.Prj_Name := +"main";
   Prj.Output_Dir := +Instr_Dir.Directory_Name;
   Prj.Spec_Suffix :=
     (C_Language => +".h", CPP_Language => +".hh", others => <>);
   Prj.Body_Suffix :=
     (C_Language => +".c", CPP_Language => +".cc", others => <>);
   Prj.Compiler_Driver := (others => Context.Orig_Compiler_Driver);
   Prj.Compiler_Options := (others => Compiler_Driver_Opts);

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
               when C_Language => Create_C_Instrumenter (Instr_Config.Tag),
               when CPP_Language => Create_CPP_Instrumenter (Instr_Config.Tag),
               when others =>
                  raise Program_Error
                    with "Unsupported language for integrated"
                         & " instrumentation");

         Fullname    : constant String :=
           Ada.Directories.Full_Name (+Comp_Command.Filename);
         Simple_Name : constant String :=
           Ada.Directories.Simple_Name (+Comp_Command.Filename);
         Instr_Name  : constant String := (+Prj.Output_Dir) / Simple_Name;

      begin
         --  Start by instrumenting the file as a source, if it is a unit of
         --  interest.

         if Files_Of_Interest.Contains (+Fullname) then

            --  Pass the compiler switches through the project description

            Instrument.Source
              (Instrumenter      => Instrumenter,
               Files_Of_Interest => Switches.Files_Of_Interest,
               Prj               => Prj,
               Unit_Name         => Fullname,

               --  Generate all the SID files under the same directory as the
               --  compiler wrapper as they must persist. TODO???: deal with
               --  homonym files in SID names.

               SID_Name => Compiler_Wrapper_Dir / (Simple_Name & ".sid"));

            Comp_Command_Ref.Instrumentation_Sources.Append
              (Instrumenter.Buffer_Unit
                 (Compilation_Unit'
                      (File_Based_Language, Comp_Command.Filename),
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

   --  Then, deal with the link command

   if Comp_DB.Link_Command /= No_Link_Command then
      declare
         Instrumenter : constant Language_Instrumenter'Class :=
           Create_C_Instrumenter (Instr_Config.Tag);
         --  Emit the buffers list unit as a C compilation unit as it is
         --  compilable by a C / C++ compiler, which are the languages
         --  supported by the integrated instrumentation scheme.

         Buffer_Symbols : constant String_Sets.Set :=
           Coverage_Buffer_Symbols
             (Tmp_Dir         => +Prj.Output_Dir,
              Command         => Comp_DB.Link_Command,
              Compiler_Driver => Command_Name,
              Config          => Instr_Config);
      begin
         Buffers_List_Unit :=
           Instrumenter.Emit_Buffers_List_Unit
             (Buffer_Symbols,
              Prj);
      end;
   end if;

   --  Now that we have all of the instrumentation artifacts, launch the
   --  original compiler driver command.

   declare
      Output_Dir : constant String := +Prj.Output_Dir;
      New_Args   : String_Vectors.Vector := Compiler_Driver_Opts;
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
                  Base      : constant String := Simple_Name (+Arg);
                  Fullname  : constant String := Full_Name (+Arg);
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
                 (Comp_Command.Filename, String_Vectors.Empty);

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
                       .Reference (Comp_Command.Filename)
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
                  Orig_Source   : constant Unbounded_String :=
                    Context.Source_Mapping.Element (Assembly_Command.Filename);
                  Instr_Objects : constant String_Vectors.Vector :=
                    Context.Instrumentation_Objects.Element (Orig_Source);
                  Packaged_Name : constant String :=
                    New_File
                      (Prj, "instr_" & Filename_Slug (+Orig_Source) & ".a");
                  Success : Boolean;
               begin
                  if not Instr_Objects.Is_Empty then
                     declare
                        Args_Ld : String_Vectors.Vector;
                     begin
                        Args_Ld.Append (+"-r");
                        Args_Ld.Append_Vector (Instr_Objects);
                        Args_Ld.Append (Assembly_Command.Target);
                        Args_Ld.Append (+"-o");
                        Args_Ld.Append (+Packaged_Name);
                        Run_Command
                          (Command             =>
                             +Instr_Config.Linkers.Element
                               (+Base_Name (Command_Name)),
                           Arguments           => Args_Ld,
                           Origin_Command_Name => "compiler wrapper");

                        --  Finally, replace the original object file with
                        --  the newly created library file, packaging both
                        --  the instrumented source and its coverage buffer.

                        GNAT.OS_Lib.Copy_File
                          (Packaged_Name,
                           +Assembly_Command.Target,
                           Success,
                           Mode     => GNAT.OS_Lib.Overwrite,
                           Preserve => GNAT.OS_Lib.Full);
                     end;
                  end if;
               end;
            end if;
         end loop;
      end if;
   end;

end Compiler_Wrappers.Gcc;
