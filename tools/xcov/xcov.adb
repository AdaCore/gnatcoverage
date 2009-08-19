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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with Coverage;          use Coverage;
with Decision_Map;      use Decision_Map;
with Execs_Dbase;       use Execs_Dbase;
with Elf_Files;
with Switches;          use Switches;
with Traces;            use Traces;
with Traces_Elf;        use Traces_Elf;
with Traces_Sources;    use Traces_Sources;
with Traces_Sources.Html;
with Traces_Sources.Xcov;
with Traces_Sources.Report;
with Traces_Names;
with Traces_Files;      use Traces_Files;
with Traces_Dbase;      use Traces_Dbase;
with Traces_Disa;
with Control_Flow_Graph;
with Version;
with Qemudrv;
with Qemu_Traces;
with Strings;           use Strings;
with Traces_Files_List; use Traces_Files_List;

procedure Xcov is
   Fatal_Error : exception;
   --  Cause Xcov to terminate. An error message must have been displayed
   --  before raising this exception.

   procedure Usage;
   --  Display usage information for documented commands

   procedure Usage_Dump;
   --  Display usage information for internal debugging commands

   procedure Error (Msg : String);
   --  Display Msg on stderr and set exit status to failure

   function Parse_Hex (S : String; Flag_Name : String) return Pc_Type;
   --  Comment needed???

   type Command_Type is
     (No_Command,
      Disp_Routines,
      Map_Routines,
      Dump_Trace,
      Dump_Trace_Base,
      Dump_Trace_Asm,
      Dump_Sections,
      Dump_Symbols,
      Dump_Compile_Units,
      Dump_Subprograms,
      Dump_Lines,
      Disassemble_Raw,
      Disassemble,
      Show_Graph,
      Run,
      Help,
      Help_Dump,
      Version);

   function To_Command (Opt_String : String) return Command_Type;
   --  Convert a string of the form "--com-mand" to the corresponding
   --  Command literal Com_Mand. No_Command is returned when no matching
   --  literal exists.

   function To_Switch (Command : Command_Type) return String;
   --  Return the command-line switch form of Command

   type Annotation_Format is
     (Annotate_Asm,
      Annotate_Xcov,
      Annotate_Html,
      Annotate_Xcov_Asm,
      Annotate_Html_Asm,
      Annotate_Report,
      Annotate_Unknown);

   function To_Annotation_Format (Option : String) return Annotation_Format;
   --  Convert annotation format option to Annotation_Format value

   Arg_Index : Natural;
   Arg_Count : constant Natural := Argument_Count;

   procedure Check_Argument_Available
     (What    : String;
      Command : Command_Type := No_Command);
   --  Check that Arg_Index is not greater than Arg_Count. If not, display
   --  an error message and raise Fatal_Error.

   function Option_Parameter (S : String) return String;
   --  Assuming that S is of the form "<part1>=<part2>",
   --  return "<part2>".

   function Begins_With (S : String; Beginning : String) return Boolean;
   --  If the beginning of S is equal to Beginnning, return True;
   --  otherwise, return False.

   -----------------
   -- Begins_With --
   -----------------

   function Begins_With (S : String; Beginning : String) return Boolean is
      Length : constant Integer := Beginning'Length;
   begin
      return S'Length > Length
        and then S (S'First .. S'First + Length - 1) = Beginning;
   end Begins_With;

   ------------------------------
   -- Check_Argument_Available --
   ------------------------------

   procedure Check_Argument_Available
     (What    : String;
      Command : Command_Type := No_Command)
   is
      function For_Command_Switch return String;
      --  Generate command indication if Command is not No_Command

      ------------------------
      -- For_Command_Switch --
      ------------------------

      function For_Command_Switch return String is
      begin
         if Command = No_Command then
            return "";
         else
            return "for " & To_Switch (Command);
         end if;
      end For_Command_Switch;

   --  Start of processing for Check_Argument_Available

   begin
      if Arg_Index > Arg_Count then
         Error ("missing " & What & " argument" & For_Command_Switch);
         raise Fatal_Error;
      end if;
   end Check_Argument_Available;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Command_Name & ": " & Msg);
      Set_Exit_Status (Failure);
   end Error;

   ----------------------
   -- Option_Parameter --
   ----------------------

   function Option_Parameter (S : String) return String is
   begin
      for J in S'Range loop
         if S (J) = '=' then
            return S (J + 1 .. S'Last);
         end if;
      end loop;
      return S;
   end Option_Parameter;

   ---------------
   -- Parse_Hex --
   ---------------

   function Parse_Hex (S : String; Flag_Name : String) return Pc_Type
   is
      Res : Pc_Type;
      Pos : Natural;
   begin
      if S'Length < 3
        or else S (S'First) /= '0'
        or else (S (S'First + 1) /= 'x' and then S (S'First + 1) /= 'X')
      then
         Error ("missing '0x' prefix for " & Flag_Name);
         raise Constraint_Error;
      end if;
      Pos := S'First + 2;
      Get_Pc (Res, S, Pos);
      if Pos <= S'Last then
         Error ("bad hexadecimal number for " & Flag_Name);
      end if;
      return Res;
   end Parse_Hex;

   --------------------------
   -- To_Annotation_Format --
   --------------------------

   function To_Annotation_Format (Option : String) return Annotation_Format is
   begin
      if Option = "asm" then
         return Annotate_Asm;
      elsif Option = "xcov" then
         return Annotate_Xcov;
      elsif Option = "html" then
         return Annotate_Html;
      elsif Option = "xcov+asm" then
         return Annotate_Xcov_Asm;
      elsif Option = "html+asm" then
         return Annotate_Html_Asm;
      elsif Option = "report" then
         return Annotate_Report;
      else
         return Annotate_Unknown;
      end if;
   end To_Annotation_Format;

   ----------------
   -- To_Command --
   ----------------

   function To_Command (Opt_String : String) return Command_Type is
      Literal : String (Opt_String'First + 2 .. Opt_String'Last) :=
                  Opt_String (Opt_String'First + 2 .. Opt_String'Last);
   begin
      if Opt_String (Opt_String'First .. Opt_String'First + 1) /= "--" then
         return No_Command;
      end if;

      for J in Literal'Range loop
         if Literal (J) = '-' then
            Literal (J) := '_';
         end if;
      end loop;

      begin
         return Command_Type'Value (Literal);
      exception
         when Constraint_Error =>
            return No_Command;
      end;
   end To_Command;

   ---------------
   -- To_Switch --
   ---------------

   function To_Switch (Command : Command_Type) return String is
      Result : String := "--" & To_Lower (Command'Img);
   begin
      for J in Result'Range loop
         if Result (J) = '_' then
            Result (J) := '-';
         end if;
      end loop;
      return Result;
   end To_Switch;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Usage: " & Command_Name & " ACTION");
      P ("Action is one of:");
      P (" --help  -h");
      P ("   Display this help");
      New_Line;
      P (" --help-dump");
      P ("   Display extra help for dump commands (useful for maintainance)");
      New_Line;
      P (" --version");
      P ("   Display version");
      New_Line;
      Qemudrv.Help (" ");
      New_Line;
      P (" --disp-routines {[--exclude|--include] FILES}");
      P ("    Build a list of routines from object files");
      New_Line;
      P (" --coverage=["
         & All_Known_Coverage_Levels & "] OPTIONS TRACE_FILES");
      P ("   Generate coverage report");
      P ("   -l FILE  --routine-list=FILE  Get routine names from LIST");
      P ("   -a FORM  --annotate=FORM      Generate a FORM report");
      P ("      FORM is one of asm,xcov,html,xcov+asm,html+asm,report");
      New_Line;
   end Usage;

   ----------------
   -- Usage_Dump --
   ----------------

   procedure Usage_Dump is
      procedure P (S : String) renames Put_Line;
   begin
      P ("Debugging commands:");
      New_Line;
      P (" --dump-trace FILES");
      P ("   Raw display of trace files");
      New_Line;
      P (" --dump-trace-base FILES");
      P ("   Raw display of merged trace files");
      New_Line;
      P (" --dump-trace-asm EXE TRACE_FILES");
      P ("   Raw display of trace files with assembly code for each trace");
      New_Line;
      P (" --dump-sections EXEs");
      P (" --dump-symbols EXEs");
      P (" --dump-compile-units EXEs");
      P (" --dump-subprograms EXEs");
      P (" --dump-lines EXEs");
      P ("   Dump info from executable files");
      New_Line;
      P (" --disassemble EXEs");
      P (" --disassemble-raw EXEs");
      P ("   Disassemble executables");
      New_Line;
   end Usage_Dump;

   Command : Command_Type := No_Command;
   Base : aliased Traces_Base;
   Exec : aliased Exe_File_Type;

   Annotations           : Annotation_Format := Annotate_Unknown;
   Annotate_Option       : constant String := "--annotate=";
   Annotate_Option_Short : constant String := "-a";

   Routine_List_Option       : constant String := "--routine-list=";
   Routine_List_Option_Short : constant String := "-l";

   ALI_List_Option           : constant String := "--ali-list=";

   Level                     : Coverage_Level;

   Final_Report_Option       : constant String := "--report=";
   Final_Report_Option_Short : constant String := "-o";

   Text_Start : Pc_Type := 0;
   Trace_File : Trace_File_Element_Acc;

begin
   --  Require at least one argument

   if Arg_Count = 0 then
      Usage;
      return;
   end if;

   --  Decode command line up to first non-option argument

   Arg_Index := 1;
   while Arg_Index <= Arg_Count loop
      declare
         Arg : String renames Argument (Arg_Index);
         Arg_Command : Command_Type := To_Command (Arg);

         function Next_Arg (What : String) return String;
         --  Increment Arg_Index then return Argument (Arg_Index). If
         --  end of command line is reached, display an error message and
         --  raise Constraint_Error.

         --------------
         -- Next_Arg --
         --------------

         function Next_Arg (What : String) return String is
         begin
            Arg_Index := Arg_Index + 1;
            Check_Argument_Available (What);
            return Argument (Arg_Index);
         end Next_Arg;

      begin
         --  Special case: command aliases

         if Arg = "-h" then
            Arg_Command := Help;
         end if;

         if Arg_Command /= No_Command then
            if Command /= No_Command then
               Error ("only one command may be specified");
               return;
            end if;
            Command := Arg_Command;

            --  Special case for Disp_Routines and Run: further command
            --  line switches are processed outside of this loop.

            if Command = Disp_Routines or else Command = Run then
               Arg_Index := Arg_Index + 1;
               exit;
            end if;

         elsif Arg = "-v" then
            Verbose := True;

         elsif Arg = Coverage_Option_Short then
            Level := To_Coverage_Level (Next_Arg ("coverage level"));
            if Level = Unknown then
               Error ("bad parameter for " & Coverage_Option_Short);
               return;
            end if;
            Set_Coverage_Level (Level);

         elsif Begins_With (Arg, Coverage_Option) then
            Level := To_Coverage_Level (Option_Parameter (Arg));
            if Level = Unknown then
               Error ("bad parameter for " & Coverage_Option);
               return;
            end if;
            Set_Coverage_Level (Level);

         elsif Begins_With (Arg, ALI_List_Option) then
            ALI_List_Filename :=
              new String'(Option_Parameter (Arg));

         elsif Arg = Routine_List_Option_Short then
            Routine_List_Filename := new String'(Next_Arg ("function list"));

         elsif Begins_With (Arg, Routine_List_Option) then
            Routine_List_Filename := new String'(Option_Parameter (Arg));

         elsif Arg = "--missing-files" then
            Flag_Show_Missing := True;

         elsif Begins_With (Arg, "--text-start=") then
            --  FIXME: not yet supported???
            --  Should be a global option (used when building decision map
            --  for --run)???

            begin
               Text_Start := Parse_Hex
                 (Arg (Arg'First + 13 .. Arg'Last), "--text-start");
            exception
               when Constraint_Error =>
                  return;
            end;

         elsif Begins_With (Arg, "--source-rebase=") then
            declare
               Pos : Natural := 0;
            begin
               for I in Arg'First + 16 .. Arg'Last loop
                  if Arg (I) = '=' then
                     Pos := I;
                     exit;
                  end if;
               end loop;
               if Pos = 0 then
                  Error ("missing '=' in --source-rebase=");
                  return;
               end if;
               Add_Source_Rebase (Arg (Arg'First + 16 .. Pos - 1),
                                  Arg (Pos + 1 .. Arg'Last));
            end;

         elsif Begins_With (Arg, "--source-search=") then
            Add_Source_Search (Arg (Arg'First + 16 .. Arg'Last));

         elsif Arg = Annotate_Option_Short then
            Annotations :=
              To_Annotation_Format (Next_Arg ("annotation format"));
            if Annotations = Annotate_Unknown then
               Error ("bad parameter for " & Annotate_Option_Short);
               return;
            end if;

         elsif Begins_With (Arg, Annotate_Option) then
            Annotations := To_Annotation_Format (Option_Parameter (Arg));
            if Annotations = Annotate_Unknown then
               Error ("bad parameter for " & Annotate_Option);
               return;
            end if;

         elsif Arg = Final_Report_Option_Short then
            Traces_Sources.Report.Open_Report_File
              (Next_Arg ("final report name"));

         elsif Begins_With (Arg, Final_Report_Option) then
            Traces_Sources.Report.Open_Report_File (Option_Parameter (Arg));

         elsif Arg (1) = '-' then
            Error ("unknown option: " & Arg);
            return;

         else
            exit;
         end if;
      end;
      Arg_Index := Arg_Index + 1;
   end loop;

   --  Now execute the specified command

   case Command is
      when Disp_Routines =>
         for J in Arg_Index + 1 .. Arg_Count loop
            declare
               Arg : constant String := Argument (J);
               Mode_Exclude : Boolean := False;
            begin
               if Arg = "--exclude" then
                  Mode_Exclude := True;
               elsif Arg = "--include" then
                  Mode_Exclude := False;
               else
                  Traces_Elf.Read_Routines_Name
                    (Arg,
                     Exclude   => Mode_Exclude,
                     Keep_Open => False);
               end if;
            exception
               when Elf_Files.Error =>
                  Error ("can't open: " & Arg);
                  return;
            end;
         end loop;
         Traces_Names.Disp_All_Routines;
         return;

      when Map_Routines =>
         Check_Argument_Available ("EXEC", Command);

         Build_Decision_Map (Argument (Arg_Index));
         return;

      when Dump_Trace =>
         Check_Argument_Available ("TRACEFILEs", Command);

         for J in Arg_Index .. Arg_Count loop
            Dump_Trace_File (Argument (J));
         end loop;
         return;

      when Dump_Trace_Base =>
         Check_Argument_Available ("TRACEFILEs", Command);

         for J in Arg_Index .. Arg_Count loop
            Trace_File := new Trace_File_Element;
            Read_Trace_File (Argument (J), Trace_File.Trace, Base);
            Dump_Traces (Base);
         end loop;
         return;

      when Dump_Trace_Asm =>
         Check_Argument_Available ("EXEC", Command);

         Open_File (Exec, Argument (Arg_Index), Text_Start);
         Build_Sections (Exec);
         Build_Symbols (Exec'Unchecked_Access);

         Arg_Index := Arg_Index + 1;
         Check_Argument_Available ("TRACEFILEs", Command);

         for J in Arg_Index .. Arg_Count loop
            Traces_Disa.Dump_Traces_With_Asm (Exec, Argument (J));
         end loop;
         return;

      when Dump_Sections | Dump_Symbols | Dump_Subprograms | Dump_Lines =>
         Check_Argument_Available ("EXECs", Command);

         declare
            To_Display : Addresses_Kind;
         begin
            for J in Arg_Index .. Arg_Count loop
               Open_File (Exec, Argument (J), 0);
               Build_Sections (Exec);

               case Command is
                  when Dump_Sections =>
                     To_Display := Section_Addresses;

                  when Dump_Symbols =>
                     Build_Symbols (Exec'Unchecked_Access);
                     To_Display := Symbol_Addresses;

                  when Dump_Subprograms =>
                     Build_Debug_Compile_Units (Exec);
                     To_Display := Subprogram_Addresses;

                  when Dump_Lines =>
                     Build_Debug_Lines (Exec);
                     To_Display := Line_Addresses;

                  when others =>
                     --  Never happens

                     raise Program_Error;
               end case;

               Disp_Addresses (Exec, To_Display);
               Close_File (Exec);
            end loop;
         end;
         return;

      when Dump_Compile_Units =>
         Check_Argument_Available ("EXECs", Command);

         for J in Arg_Index  .. Arg_Count loop
            Open_File (Exec, Argument (J), 0);
            Build_Sections (Exec);
            Build_Debug_Compile_Units (Exec);
            Disp_Compilation_Units (Exec);
            Close_File (Exec);
         end loop;
         return;

      when Disassemble_Raw =>
         Check_Argument_Available ("EXECs", Command);

         for J in Arg_Index .. Arg_Count loop
            Open_File (Exec, Argument (J), 0);
            Disassemble_File_Raw (Exec);
            Close_File (Exec);
         end loop;
         return;

      when Disassemble =>
         Check_Argument_Available ("EXECs", Command);

         for J in Arg_Index .. Arg_Count loop
            Open_File (Exec, Argument (J), 0);
            Build_Sections (Exec);
            Build_Symbols (Exec'Unchecked_Access);
            Disassemble_File (Exec);
            Close_File (Exec);
         end loop;
         return;

      when Show_Graph =>
         Check_Argument_Available ("EXEC", Command);

         Open_File (Exec, Argument (Arg_Index), Text_Start);
         Build_Sections (Exec);
         Build_Symbols (Exec'Unchecked_Access);
         Build_Debug_Lines (Exec);
         Control_Flow_Graph.Generate_Graph (Exec);
         return;

      when Run =>
         --  Arg_Index is designating the argument immediately following --run

         Qemudrv.Driver (Arg_Index);
         return;

      when Version =>
         Put_Line ("XCOV Pro " & Standard.Version.Xcov_Version);
         return;

      when Help =>
         Usage;
         return;

      when Help_Dump =>
         Usage_Dump;
         return;

      when No_Command =>
         null;
   end case;

   --  Here if no command is provided: perform coverage analysis

   Check_Argument_Available ("TRACEFILEs");

   case Get_Coverage_Level is
      when Insn =>
         --  Nothing left to be done after having called Set_Coverage_Level;
         --  Set_Insn_State will call Get_Coverage_Level again to determine
         --  what object coverage objective should be used.
         null;

      when Branch =>
         --  Ditto.
         null;

      when Stmt =>
         Put_Line ("Stmt coverage has not been implemented yet.");
         return;

      when Decision =>
         Put_Line ("Decision coverage has not been implemented yet.");
         return;

      when MCDC =>
         Put_Line ("MCDC coverage has not been implemented yet.");
         return;

      when Unknown =>
         Error ("Please specify a coverage level.");
         return;

   end case;

   if Routine_List_Filename /= null then
      Traces_Names.Read_Routines_Name_From_Text (Routine_List_Filename.all);
   else
      if Arg_Index < Arg_Count then
         Error ("routine list required when reading multiple trace files");
         return;
      end if;
   end if;

   --  Read traces

   while Arg_Index <= Arg_Count loop
      Init_Base (Base, Full_History => Get_Coverage_Level = MCDC);
      Trace_File := new Trace_File_Element;
      Trace_File.Filename := new String'(Argument (Arg_Index));
      Read_Trace_File (Trace_File.Filename.all,
                       Trace_File.Trace, Base);
      Traces_Files_List.Files.Append (Trace_File);
      declare
         Exe_Name : constant String :=
           Get_Info (Trace_File.Trace, Qemu_Traces.Exec_File_Name);
         Exe_File : Exe_File_Acc;
      begin
         if Exe_Name = "" then
            Error ("cannot find exec filename in trace file "
                     & Argument (Arg_Index));
            return;
         end if;
         begin
            Open_Exec (Get_Exec_Base, Exe_Name, Exe_File);
         exception
            when Elf_Files.Error =>
               Error ("cannot open ELF file " & Exe_Name & " for trace file "
                        & Argument (Arg_Index));
               return;
         end;

         --  If there is not routine list, create it from the first executable.
         --  A test above allows this only if there is one trace file.

         if Routine_List_Filename = null then
            Read_Routines_Name (Exe_File, Exclude => False);
         end if;

         Load_Code_And_Traces (Exe_File, Base'Access);
      end;
      Arg_Index := Arg_Index + 1;
   end loop;

   Traces_Elf.Build_Routines_Insn_State;

   if Annotations /= Annotate_Asm then
      Traces_Elf.Build_Source_Lines;
      --  This annotates sources with object coverage information, so
      --  definitely wrong in the case of source coverage???
   end if;

   case Annotations is
      when Annotate_Asm =>
         if Get_Coverage_Level in Source_Coverage_Level then
            Error ("Asm format not supported for source coverage.");
            return;
         end if;
         Traces_Disa.Flag_Show_Asm := True;
         Coverage.Dump_Coverage_Option (Standard_Output);
         Traces_Sources.Dump_Routines_Traces;

      when Annotate_Xcov =>
         Traces_Sources.Xcov.Generate_Report (False);

      when Annotate_Html =>
         Traces_Sources.Html.Generate_Report (False);

      when Annotate_Xcov_Asm =>
         --  Case of source coverage???
         Traces_Sources.Xcov.Generate_Report (True);

      when Annotate_Html_Asm =>
         --  Case of source coverage???
         Traces_Sources.Html.Generate_Report (True);

      when Annotate_Report =>
         Coverage.Dump_Coverage_Option (Traces_Sources.Report.Get_Output);
         Traces_Sources.Dump_Uncovered_Routines
           (Traces_Sources.Report.Get_Output);
         Traces_Sources.Report.Finalize_Report;

      when Annotate_Unknown =>
         Put_Line ("Please specify an annotation format.");
         return;
   end case;

exception
   when Fatal_Error =>
      --  An error message has already been displayed

      null;
end Xcov;
