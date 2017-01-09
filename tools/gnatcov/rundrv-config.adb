------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2016, AdaCore                       --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Directories;   use Ada.Directories;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Regpat; use GNAT.Regpat;
with GNAT.OS_Lib;

with Arch;
with Coverage; use Coverage;
with Files_Table;
with Switches; use Switches;

package body Rundrv.Config is

   function Gnatemu_Driver (Context : Context_Type) return Command_Access;
   --  Helper for Lookup_Driver. If there is a GNATemulator available for this
   --  target, return a command to run it. Return null otherwise.

   function Builtin_Driver (Context : Context_Type) return Command_Access;
   --  Helper for Lookup_Driver. If there is a builtin driver available for
   --  this target, return a command to run it. Return null otherwise.

   -------------------------------------------
   -- Matching helpers for built-in targets --
   -------------------------------------------

   type Driver_Creator_Type is access function
     (Context : Context_Type;
      Matches : Match_Array)
      return Command_Access;
   --  Function that creates a command for some target.
   --
   --  Such functions are evaluated when the target family string matches some
   --  pattern. Context contains the information used to select such a
   --  function and Matches contains the matched substrings in the target
   --  family.
   --
   --  Note that the returned command is not expected to contain arguments for
   --  the input program (eargs): the caller will append them at the end of the
   --  arguments list.

   function Native_Linux
     (Context : Context_Type; Matches : Match_Array) return Command_Access;
   function Native_Windows
     (Context : Context_Type; Matches : Match_Array) return Command_Access;
   function ISystem
     (Context : Context_Type; Matches : Match_Array) return Command_Access;
   function Visium_ELF
     (Context : Context_Type; Matches : Match_Array) return Command_Access;

   type Builtin_Target_Type is record
      Pattern        : Ada.Strings.Unbounded.Unbounded_String;
      --  Pattern used to match a target family string

      Driver_Creator : Driver_Creator_Type;
      --  Function that creates a command for this target
   end record;
   --  Descriptor for a handled builtin target

   type Builtin_Targets_Type is
      array (Positive range <>) of Builtin_Target_Type;

   Builtin_Targets : constant Builtin_Targets_Type :=
     ((+"(i686|x86_64).*linux", Native_Linux'Access),
      (+"(i686|x86_64).*mingw", Native_Windows'Access),
      (+"(x86|x86_64)-linux",   Native_Linux'Access),
      (+"(x86|x86_64)-windows", Native_Windows'Access),
      (+"iSystem-(5554|5634)",  ISystem'Access),
      (+"visium-elf",           Visium_ELF'Access));
   --  For each target category, this table provides a target triples
   --  (without board name) pattern and a function to create the corresponding
   --  driver.

   --------------------------------
   -- Command formatting helpers --
   --------------------------------

   Gnatcov_Dir : constant String := Containing_Directory
     (GNAT.OS_Lib.Locate_Exec_On_Path (Command_Name).all);

   Gnatcov_Prefix : constant String
     := Containing_Directory (Gnatcov_Dir);

   Libexec_Dir : constant String := Gnatcov_Prefix & "/libexec/gnatcoverage";

   procedure Append_Arg (Cmd : Command_Access; Arg : String);
   procedure Append_Arg (Cmd : Command_Access; Opt, Arg : String);

   function Trace_Input (Context : Context_Type) return String;
   --  Return the standard argument scheme to embed histmap file and traces
   --  file when relevant.

   function Bits return String;
   --  String representing the target architecture address size in bits

   function Bundled_Or_Plain (What, Where : String) return String;
   --  Return the PATH to use to reference WHAT, an executable or a shared
   --  library possibly located in the WHERE subdir of our local libexec tree.
   --  If <libexec>/WHERE/WHAT exists, return that.  Return WHAT otherwise.

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg (Cmd : Command_Access; Arg : String) is
   begin
      Cmd.Arguments.Append (+Arg);
   end Append_Arg;

   ----------------
   -- Append_Arg --
   ----------------

   procedure Append_Arg (Cmd : Command_Access; Opt, Arg : String) is
   begin
      Append_Arg (Cmd, Opt);
      Append_Arg (Cmd, Arg);
   end Append_Arg;

   ----------
   -- Bits --
   ----------

   function Bits return String is
   begin
      return Trim (Integer'Image (Arch.Arch_Addr'Object_Size), Left);
   end Bits;

   ----------------------
   -- Bundled_Or_Plain --
   ----------------------

   function Bundled_Or_Plain (What, Where : String) return String is
      Dir     : constant String :=
         Files_Table.Build_Filename (Libexec_Dir, Where);
      Bundled : constant String :=
         Files_Table.Build_Filename (Dir, What);
   begin
      return (if Exists (Bundled) then Bundled else What);
   end Bundled_Or_Plain;

   -----------------
   -- Trace_Input --
   -----------------

   function Trace_Input (Context : Context_Type) return String is
   begin
      if Context.Histmap /= null then
         return ("histmap=" & Context.Histmap.all
                 & ',' & Context.Trace_File.all);

      elsif MCDC_Coverage_Enabled then
         return "history," & Context.Trace_File.all;

      else
         return Context.Trace_File.all;
      end if;
   end Trace_Input;

   ------------------
   -- Native_Linux --
   ------------------

   function Native_Linux
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      pragma Unreferenced (Matches);

      Cmd    : constant String :=
         Bundled_Or_Plain (What => "valgrind", Where => "bin");
      Result : constant Command_Access := new Command_Type'
        (Command => +Cmd, others => <>);
   begin
      if not Verbose then
         Append_Arg (Result, "--quiet");
      end if;
      Append_Arg (Result, "--tool=coverage");
      Append_Arg (Result, "--cov-exec-file=" & Trace_Input (Context));
      Append_Arg (Result, Context.Exe_File.all);

      --  If we're using our bundled-in valgrind, adjust VALGRIND_LIB
      --  accordingly.

      if Cmd /= "valgrind" then
         Result.Environment.Insert (+"VALGRIND_LIB",
                                    +(Libexec_Dir & "/lib/valgrind/"));
      end if;
      return Result;
   end Native_Linux;

   --------------------
   -- Native_Windows --
   --------------------

   function Native_Windows
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      pragma Unreferenced (Matches);

      Drrun    : constant String :=
         Bundled_Or_Plain (What => "drrun.exe", Where => "bin" & Bits);
      Drclient : constant String :=
         Bundled_Or_Plain (What => "qtrace.dll", Where => "lib" & Bits);
      Result : constant Command_Access := new Command_Type'
        (Command => +Drrun, others => <>);
   begin
      --  -quiet silences the warnings emitted by DynamoRIO on the assumption
      --  that it is invoked from an official release install tree.

      if not Verbose then
         Append_Arg (Result, "-quiet");
      end if;
      Append_Arg (Result, "-no_follow_children");
      Append_Arg (Result, "-c", Drclient);
      Append_Arg (Result, "-o", Trace_Input (Context));
      Append_Arg (Result, "--", Context.Exe_File.all);
      return Result;
   end Native_Windows;

   -------------
   -- ISystem --
   -------------

   function ISystem
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      Result : constant Command_Access := new Command_Type'
        (Command => +"../libexec/gnatcoverage/isys_drv", others => <>);
      System : constant String :=
        Context.Target_Family.all (Matches (1).First .. Matches (1).Last);
   begin
      Append_Arg (Result, System);
      Append_Arg (Result, Context.Exe_File.all);
      Append_Arg (Result, Trace_Input (Context));
      return Result;
   end ISystem;

   ----------------
   -- Visium_ELF --
   ----------------

   function Visium_ELF
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      pragma Unreferenced (Matches);

      Result : constant Command_Access := new Command_Type'
        (Command => +"visium-elf-run", others => <>);
   begin
      Append_Arg (Result, "--trace=" & Context.Trace_File.all);
      Append_Arg (Result, Context.Exe_File.all);
      return Result;
   end Visium_ELF;

   --------------------
   -- Gnatemu_Driver --
   --------------------

   function Gnatemu_Driver (Context : Context_Type) return Command_Access
   is
      Gnatemu : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path
        (Context.Target_Family.all & "-gnatemu");
      Result  : Command_Access := new Command_Type;
   begin

      if Gnatemu = null then
         Free (Result);
         return null;
      end if;

      Result.Command := +Gnatemu.all;
      Free (Gnatemu);

      --  Compute the subsets of options we need to pass.  As common options,
      --  we always need to pass the executable name to Gnatemu (last), and
      --  request the production of traces straight to the underlying emulator
      --  in addition to our own -eargs. Then we have the possible --kernel
      --  and --board extensions, and the project file related options.

      if Context.Kernel /= null then
         Append_Arg (Result, "--kernel=" & Context.Kernel.all);
      end if;

      if Context.Target_Board /= null then
         Append_Arg (Result, "--board=" & Context.Target_Board.all);
      end if;

      if Root_Project /= null then
         Append_Arg (Result, "-P", Root_Project.all);

         declare
            use Key_Element_Maps;
         begin
            for Scenario_Var in S_Variables.Iterate loop
               Append_Arg (Result, "-X" & Key (Scenario_Var)
                                   & "=" & Element (Scenario_Var));
            end loop;
         end;
      end if;

      Append_Arg (Result, "--eargs");
      Append_Arg (Result, "-exec-trace", Trace_Input (Context));
      for Earg of Context.Eargs.all loop
         Append_Arg (Result, Earg.all);
      end loop;
      Append_Arg (Result, "--eargs-end");
      Append_Arg (Result, Context.Exe_File.all);

      return Result;
   end Gnatemu_Driver;

   --------------------
   -- Builtin_Driver --
   --------------------

   function Builtin_Driver (Context : Context_Type) return Command_Access is
      Result : Command_Access;
   begin
      for T of Builtin_Targets loop
         declare
            Pattern : constant Pattern_Matcher := Compile (+T.Pattern);
            Matches : Match_Array (0 .. Paren_Count (Pattern));
         begin
            Match (Pattern, Context.Target_Family.all, Matches);
            if Matches (0) /= No_Match then
               Result := T.Driver_Creator (Context, Matches);

               --  Always append the eargs at the end of the command line

               for Earg of Context.Eargs.all loop
                  Append_Arg (Result, Earg.all);
               end loop;
               return Result;
            end if;
         end;
      end loop;

      return null;
   end Builtin_Driver;

   -------------------
   -- Lookup_Driver --
   -------------------

   function Lookup_Driver (Context : Context_Type) return Command_Access is
      Result : Command_Access;
   begin
      --  If there is a GNATemulator available, just use it

      Result := Gnatemu_Driver (Context);
      if Result /= null then
         return Result;
      end if;

      --  Otherwise, fall back to our knowledge base

      Result := Builtin_Driver (Context);
      if Result /= null then
         return Result;
      end if;

      return null;
   end Lookup_Driver;

   -----------------------
   -- Available_Targets --
   -----------------------

   function Available_Targets return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      First  : Boolean := True;
   begin
      for T of Builtin_Targets loop
         if not First then
            Append (Result, ", ");
         end if;
         First := False;

         Append (Result, +T.Pattern);
      end loop;
      return To_String (Result);
   end Available_Targets;

end Rundrv.Config;
