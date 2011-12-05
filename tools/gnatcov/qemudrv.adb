------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2009-2011, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;

with Ada.Unchecked_Conversion;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;

with Ada.Containers.Vectors;

with Interfaces;

with GNAT.OS_Lib;

with Coverage;     use Coverage;
with Qemu_Traces;
with Qemudrv_Base; use Qemudrv_Base;
with Switches;     use Switches;
with Traces_Files; use Traces_Files;

package body Qemudrv is

   Target_Default : constant String_Access := new String'("powerpc-elf");

   --  Variables set by the command line.

   Trace_Output : String_Access;
   --  Trace output filename

   Histmap_Filename : String_Access;
   --  File name of history map or null if none.

   Executable : String_Access;
   --  Executable to run

   Exec_Error : exception;
   --  Raised when subprogram execution failed. The error message shall be
   --  generated before raising the exception.

   procedure Error (Msg : String);
   --  Display the message on the error output and set exit status

   function Expand_Arguments
     (Args, Eargs : String_List_Access) return String_List;
   --  Expand macro arguments in ARGS and hook EARGS into a complete list
   --  of real arguments to pass to the immediate execution environment.

   procedure Run_Command (Command : String_Access; Options : String_List);
   --  Spawn command with options

   ------------
   -- Driver --
   ------------

   procedure Driver
     (Exe_File : String;
      Target   : String_Access;
      Tag      : String_Access;
      Output   : String_Access;
      Histmap  : String_Access;
      Eargs    : String_List_Access)
   is
      type Driver_Target_Access is access constant Driver_Target;

      function Driver_Control_For
        (Target : String_Access) return Driver_Target_Access;
      --  Return an access to the Driver_Target control block to use for
      --  TARGET. This will be a <target>-gnatemu block if GNATemulator is
      --  available on PATH, or a low-level emulator block from our static
      --  configuration table otherwise. Return null if we can't figure out
      --  any sensible control block for the provided target name.

      ------------------------
      -- Driver_Control_For --
      ------------------------

      function Driver_Control_For
        (Target : String_Access) return Driver_Target_Access is
      begin

         --  If we have GNATemulator for Target on PATH, use that. --target
         --  values provided by users are expected to match the GNATemulator
         --  target names in such cases, so there's no point looking into
         --  the Aliases entries here.

         declare
            Gnatemu : constant String_Access
              := GNAT.OS_Lib.Locate_Exec_On_Path (Target.all & "-gnatemu");
         begin

            if Gnatemu /= null then

               --  We just need to pass the executable name to Gnatemu (last),
               --  and request the production of traces straight to the
               --  underlying emulator in addition to our own -eargs.

               return new Driver_Target'
                 (Target => Target,
                  Build_Command => null,
                  Build_Options => null,
                  Run_Command => new String'(Gnatemu.all),
                  Run_Options => new String_List'
                    (new String'("--eargs"),
                     new String'("-exec-trace"),
                     new String'("%trace"),
                     new String'("%eargs"),
                     new String'("--eargs-end"),
                     new String'("%exe"))
                 );
            end if;
         end;

         --  Otherwise, see if we have a bare emulator entry for Target. The
         --  target name might be a generic alias for a low-level board name
         --  in this case.

         declare
            Resolved_Target : String_Access := Target;
         begin

            --  Resolve against our target board Aliases table first,
            --  then seek a matching Driver entry.

            for I in Aliases'Range loop
               if Resolved_Target.all = Aliases (I).Alias.all then
                  Resolved_Target := Aliases (I).Target;
               end if;
            end loop;

            for I in Drivers'Range loop
               if Drivers (I).Target.all = Resolved_Target.all then
                  return Drivers (I)'Access;
               end if;
            end loop;
         end;

         --  Nothing we can do if we haven't found anything at this point

         return null;

      end Driver_Control_For;

      Real_Target : not null String_Access := Target_Default;
      --  What we should be using as the target name, possibly not
      --  provided on entry.

      Control : Driver_Target_Access;
      --  The corresponding Driver_Target control block

   begin

      --  Setup our basic internal control parameters

      if Target /= null then
         Real_Target := Target;
      end if;

      Control := Driver_Control_For (Real_Target);

      if Control = null then
         Error ("unknown target " & Real_Target.all);
         Error (" (use --help to get target list)");
         --  ??? xcov run --help should give the target list
         return;
      end if;

      --  Setup global state

      if Output /= null then
         Trace_Output := Output;
      else
         Trace_Output := new String'(Simple_Name (Exe_File & ".trace"));
      end if;

      Histmap_Filename := Histmap;

      Executable := new String'(Exe_File);

      --  Create the trace file

      declare
         use GNAT.OS_Lib;
         use Qemu_Traces;
         use Interfaces;
         Trace_File : Trace_File_Type;
         Date_Info  : Trace_Info_Date;
         Date       : constant OS_Time := Current_Time;
         subtype String_8 is String (1 .. 8);
         function Date_Info_To_Str is new Ada.Unchecked_Conversion
           (Trace_Info_Date, String_8);
      begin
         Create_Trace_File (Info, Trace_File);
         Date_Info := Trace_Info_Date'(Year  => Unsigned_16 (GM_Year (Date)),
                                       Month => Unsigned_8 (GM_Month (Date)),
                                       Day   => Unsigned_8 (GM_Day (Date)),
                                       Hour  => Unsigned_8 (GM_Hour (Date)),
                                       Min   => Unsigned_8 (GM_Minute (Date)),
                                       Sec   => Unsigned_8 (GM_Second (Date)),
                                       Pad   => 0);
         Append_Info (Trace_File, Date_Time, Date_Info_To_Str (Date_Info));
         Append_Info (Trace_File, Exec_File_Name, Exe_File);

         if GNAT.Strings."/=" (Tag, null) then
            Append_Info (Trace_File, User_Data, Tag.all);
         end if;

         Write_Trace_File (Trace_Output.all, Trace_File);
         Free (Trace_File);
      end;

      --  Build the executable (if necessary)

      if Control.Build_Command /= null then
         Run_Command (Control.Build_Command, Control.Build_Options.all);
      end if;

      --  If we have no command to run, we're done.  Otherwise, run it.

      if Control.Run_Command = null then
         return;
      end if;

      Run_Command (Control.Run_Command,
                   Expand_Arguments (Control.Run_Options, Eargs));

      if Verbose then
         Put (Control.Run_Command.all & " finished");
      end if;

   exception
      when Exec_Error =>
         Set_Exit_Status (Failure);
   end Driver;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
      Set_Exit_Status (Failure);
   end Error;

   ----------------------
   -- Expand_Arguments --
   ----------------------

   function Try_Expand (Arg : String) return String_Access;
   --  Try string-macro expansions on ARG.  Return a newly allocated string
   --  with the expansion performed if one applied, null oterwise.

   function Expand_Arguments
     (Args, Eargs : String_List_Access) return String_List
   is

      --  Args might include a %eargs macro or not, and in either case, we
      --  might have actual Eargs to expand or not.  We just append items as
      --  they come into a vector and convert that into a String_List
      --  eventually.

      function Length (L : String_List_Access) return Integer;
      --  Number of items in list L if not null.  Zero if L is null.

      function Length (L : String_List_Access) return Integer is
      begin
         if L = null then
            return 0;
         else
            return L'Length;
         end if;
      end Length;

      Max_N_Args : constant Integer := Length (Args) + Length (Eargs);
      --  Maximum size of the final argument vector.  All the macros expand
      --  into a single string, except eargs which expands into a string per
      --  earg.

      subtype Args_Index is Integer range 1 .. Max_N_Args;

      package String_Access_Vectors is
         new Ada.Containers.Vectors (Index_Type => Args_Index,
                                     Element_Type => String_Access);

      subtype Svector is String_Access_Vectors.Vector;

      Argv : Svector;
      --  Vector of real command line arguments to pass, past the expansion
      --  of argument macros (%exe, %trace, %eargs, ...)

      procedure Append
        (Argv : in out String_Access_Vectors.Vector; SL : String_List_Access);
      --  Append the sequence of strings in SL to the ARGV vector

      procedure Append
        (Argv : in out String_Access_Vectors.Vector;
         SL   : String_List_Access)
      is
      begin
         for I in SL'Range loop
            Argv.Append (SL (I));
         end loop;
      end Append;

      Eargs_Q : String_List_Access := Eargs;
      --  List of eargs that remains to be added to the command line

   begin
      --  Copy arguments and expand argument macros

      for J in Args'Range loop

         --  First see if we have a single-string macro to expand.  If we
         --  don't, check for %eargs.  If we have nothing to expand, just
         --  append the argument as-is.

         declare
            Exp : constant String_Access := Try_Expand (Args (J).all);
         begin
            if Exp /= null then
               Argv.Append (Exp);
            elsif Args (J).all = "%eargs" then
               if Eargs_Q /= null then
                  Append (Argv, Eargs_Q);
                  Eargs_Q := null;
               end if;
            else
               Argv.Append (Args (J));
            end if;
         end;
      end loop;

      --  If we had eargs to pass and didn't have a macro to place them
      --  specifically, add them at the end

      if Eargs_Q /= null then
         Append (Argv, Eargs_Q);
      end if;

      --  Now convert the vector into a String_List and return

      declare
         Result : String_List (1 .. Integer (Argv.Length));
      begin
         for I in Argv.First_Index .. Argv.Last_Index loop
            Result (I) := Argv (I);
         end loop;
         return Result;
      end;

   end Expand_Arguments;

   ----------
   -- Help --
   ----------

   procedure Help (Indent : String := "")
   is
      procedure P (Str : String);
      --  Put_Line with proper indentation

      -------
      -- P --
      -------

      procedure P (Str : String) is
      begin
         Put_Line (Indent & Str);
      end P;

   begin
      P ("run [OPTIONS] FILE [-eargs EARGS...]");
      P ("  -t TARGET  --target=TARGET   Set the target");
      Put (Indent & "    targets:");
      for I in Drivers'Range loop
         Put (' ');
         Put (Drivers (I).Target.all);
      end loop;
      New_Line;
      P ("  -v --verbose                 Be verbose");
      P ("  -T TAG  --tag=TAG            Put TAG in tracefile");
      P ("  -o FILE  --output=FILE       Write traces to FILE");
      P ("  -eargs EARGS                 " &
           "Pass EARGS to the low-level emulator");
   end Help;

   ------------------------
   -- Try_Expand helpers --
   ------------------------

   --  Value functions, each return the single-string value corresponding
   --  to a specific string-macro:

   --  %exe

   function Exe return String;

   function Exe return String is
   begin
      return Executable.all;
   end Exe;

   --  %bin

   function Bin return String;

   function Bin return String is
   begin
      return Executable.all & ".bin";
   end Bin;

   --  %dir_exe

   function Dir_Exe return String;

   function Dir_Exe return String is
   begin
      return Containing_Directory (Executable.all);
   end Dir_Exe;

   --  %base_bin

   function Base_Bin return String;

   function Base_Bin return String is
   begin
      return Simple_Name (Executable.all) & ".bin";
   end Base_Bin;

   --  %trace

   function Trace return String;

   function Trace return String is
   begin
      if Histmap_Filename /= null then
         return "histmap=" & Histmap_Filename.all
           & ',' & Trace_Output.all;

      elsif MCDC_Coverage_Enabled then
         return "history," & Trace_Output.all;

      else
         return Trace_Output.all;
      end if;
   end Trace;

   --  A table saying which value function to call for each macro. Better
   --  extracted out to be computed once only.

   type Smacro_Entry is record
      Key  : String_Access;
      Eval : access function return String;
   end record;

   type Smacro_Table is array (Integer range <>) of Smacro_Entry;

   SMtable : constant Smacro_Table :=
     ((Key => new String'("%exe"), Eval => Exe'Access),
      (Key => new String'("%bin"), Eval => Bin'Access),
      (Key => new String'("%dir_exe"), Eval => Dir_Exe'Access),
      (Key => new String'("%base_bin"), Eval => Base_Bin'Access),
      (Key => new String'("%trace"), Eval => Trace'Access)
     );

   ----------------
   -- Try_Expand --
   ----------------

   function Try_Expand (Arg : String) return String_Access is

      --  For earch possible string-macro, we check presence and deal
      --  with the replacement using regexp matching.

      type Macro_Matches is new Match_Array (0 .. 2);
      --  An array to hold regpat groups matching strings that precede and
      --  follow a key in an outer string, as in <pre>KEY<post>.

      function Match
        (Arg, Key : String; Matches : access Macro_Matches) return Boolean;
      --  Whether KEY is somewhere within ARG.  When it is, MATCHES (1..2)
      --  hold regpat groups describing what precedes and follows.

      function Match
        (Arg, Key : String; Matches : access Macro_Matches) return Boolean
      is
         Regexp : constant String := "^(.*)" & Key & "(.*)$";
      begin
         Match (Compile (Regexp), Arg, Matches.all);
         return Matches (0) /= No_Match;
      end Match;

      function Substitute
        (Matches : Macro_Matches; Arg, Value : String) return String;
      --  ARG is a string where a macro key was found.  MATCHES holds the
      --  regpat groups that describe what precedes and follows the key.
      --  Return a string corresponding to ARG where the macro is replaced
      --  by VALUE.

      function Substitute
        (Matches : Macro_Matches; Arg, Value : String) return String
      is
      begin
         --  When a <pre> or <post> key part is empty, we expect the
         --  corresponding group to hold an empty string that we can
         --  concatenate as well.

         return (Arg (Matches (1).First .. Matches (1).Last)
                   & Value & Arg (Matches (2).First .. Matches (2).Last));
      end Substitute;

      M : aliased Macro_Matches;

   begin

      --  Loop over the candidate macro replacements.  Substitute and
      --  return as soon we find a match.

      for I in SMtable'Range loop
         if Match (Arg, SMtable (I).Key.all, M'Access) then
            return new String'
              (Substitute (M, Arg, SMtable (I).Eval.all));
         end if;
      end loop;

      return null;
   end Try_Expand;

   -----------------
   -- Run_Command --
   -----------------

   procedure Run_Command
     (Command : String_Access; Options : String_List)
   is
      Success : Boolean;
      Prg     : String_Access;
   begin
      --  Find executable

      Prg := GNAT.OS_Lib.Locate_Exec_On_Path (Command.all);
      if Prg = null then
         Error ("gnatcov run: cannot find "
                  & Command.all
                  & " on your path");
         raise Exec_Error;
      end if;

      if Verbose then
         Put ("exec: ");
         Put (Prg.all);
         for I in Options'Range loop
            Put (' ');
            Put (Options (I).all);
         end loop;
         New_Line;
      end if;

      --  Run

      GNAT.OS_Lib.Spawn (Prg.all, Options, Success);
      if not Success then
         if Verbose then
            Error ("gnatcov run failed");
         end if;
         raise Exec_Error;
      end if;
   end Run_Command;

end Qemudrv;
