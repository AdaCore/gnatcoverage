------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;
with Ada.Command_Line; use Ada.Command_Line;

with Coverage;     use Coverage;
with GNAT.OS_Lib;

with Qemudrv.State; use Qemudrv.State;

package body Qemudrv.Expander is

   type Smt_Access is access constant Smacro_Table;

   function Try_Expand
     (Arg : String_Access; Mta : Smt_Access) return String_Access;
   --  Try string-macro expansions on ARG, replacing a key (if any) with the
   --  the corresponding function value as directed by the mapping provided
   --  through the MTA (Macro Table Access) argument.
   --
   --  Return a newly allocated string with the expansion performed if one
   --  applied, or the original ARG oterwise.

   --  This is a helper for both Expand_Arguments and Expand_Command.

   ------------------------
   -- Try_Expand helpers --
   ------------------------

   --  Value functions, each return the single-string value corresponding
   --  to a specific string-macro:

   --  %exe

   function Exe return String is
   begin
      return Executable.all;
   end Exe;

   --  %exe_dir

   function Exe_Dir return String is
   begin
      return Containing_Directory (Executable.all);
   end Exe_Dir;

   --  %tools_dir

   function Tools_Dir return String is
      Tools_Path : constant String_Access
        := GNAT.OS_Lib.Getenv ("GNATCOV_TOOLS_PATH");
   begin
      return (if Tools_Path'Length > 0
              then Tools_Path.all
              else "../libexec/gnatcoverage");
   end Tools_Dir;

   --  %trace

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

   --  %set_valgrind_env

   function Set_Valgrind_Env return String is
   begin
      GNAT.OS_Lib.Setenv
        (Name =>
           "VALGRIND_LIB",
         Value =>
           (Containing_Directory
              (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all)
            & "/../libexec/gnatcoverage")
         );

      return "";
   end Set_Valgrind_Env;

   ----------------------
   -- Expand_Arguments --
   ----------------------

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

         --  First check for %eargs. There is no possible value function for
         --  it, so there's no point trying to expand anything in this case.
         --
         --  Otherwise, see if we have anything to expand and only append to
         --  the final list if the expansion result is non-empty.

         if Args (J).all = "%eargs" then

            --  If there's an EARGS list passed in, append it here. Otherwise
            --  do nothing, which will just strip the "%eargs" entry from the
            --  final list.

            if Eargs_Q /= null then
               Append (Argv, Eargs_Q);
               Eargs_Q := null;
            end if;

         else

            declare
               Exp : constant String_Access
                 := Try_Expand (Args (J), Smtable_For_Args'Access);
            begin
               if Exp.all'Length /= 0 then
                  Argv.Append (Exp);
               end if;
            end;
         end if;
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

   ----------------------
   --  Expand_Command  --
   ----------------------

   function Expand_Command
     (Command : String_Access) return String_Access
   is
   begin
      if Command = null then
         return null;
      end if;

      declare
         Ecommand : constant String_Access
           := Try_Expand (Command, Smtable_For_Commands'Access);
      begin
         return (if Ecommand.all'Length = 0 then null else Ecommand);
      end;
   end Expand_Command;

   ----------------
   -- Try_Expand --
   ----------------

   function Try_Expand
     (Arg : String_Access; Mta : Smt_Access) return String_Access
   is

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

      --  If there's not even the start of a possible macro reference
      --  in what we're queried to expand, return immediatly.

      if not Is_In ('%', To_Set (Arg.all)) then
         return Arg;
      end if;

      --  Loop over the candidate macro replacements.  Substitute and
      --  return as soon we find a match.

      for I in Mta'Range loop
         if Match (Arg.all, Mta (I).Key.all, M'Access) then
            return new String'
              (Substitute (M, Arg.all, Mta (I).Eval.all));
         end if;
      end loop;

      return Arg;
   end Try_Expand;

end Qemudrv.Expander;
