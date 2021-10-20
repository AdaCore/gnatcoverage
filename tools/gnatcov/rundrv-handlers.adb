------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2016-2021, AdaCore                     --
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

with GNAT.OS_Lib;

with Ada.Directories;   use Ada.Directories;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Arch;
with Coverage;      use Coverage;
with Files_Table;
with Support_Files;
with Strings;       use Strings;
with Switches;      use Switches;

package body Rundrv.Handlers is

   function Bits return String;
   --  String representing the target architecture address size in bits

   function Bundled_Or_Plain (What, Where : String) return String;
   --  Return the PATH to use to reference WHAT, an executable or a shared
   --  library possibly located in the WHERE subdir of our local libexec tree.
   --  If <libexec>/WHERE/WHAT exists, return that.  Return WHAT otherwise.

   function Trace_Input (Context : Context_Type) return String;
   --  Return the standard argument scheme to embed histmap file and traces
   --  file when relevant.

   ----------------------
   -- Bundled_Or_Plain --
   ----------------------

   function Bundled_Or_Plain (What, Where : String) return String is
      Dir     : constant String :=
         Files_Table.Build_Filename (Support_Files.Libexec_Dir, Where);
      Bundled : constant String :=
         Files_Table.Build_Filename (Dir, What);
   begin
      return (if Exists (Bundled) then Bundled else What);
   end Bundled_Or_Plain;

   ----------
   -- Bits --
   ----------

   function Bits return String is
   begin
      return Trim (Integer'Image (Arch.Arch_Addr'Object_Size), Left);
   end Bits;

   -----------------
   -- Trace_Input --
   -----------------

   function Trace_Input (Context : Context_Type) return String is

      --  Normalize the trace path name we are going to provide, as
      --  some trace adapters have IO services not dealing well with
      --  possible references to relative directories within the path.

      Trace_Name : constant String :=
         GNAT.OS_Lib.Normalize_Pathname (Context.Trace_File.all);
   begin
      if Context.Histmap /= null then
         return "histmap=" & Context.Histmap.all & ',' & Trace_Name;

      elsif MCDC_Coverage_Enabled then
         return "history," & Trace_Name;

      else
         return Trace_Name;
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
        (Command => +Cmd, Native => True, others => <>);
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
         Result.Environment.Insert
           (+"VALGRIND_LIB",
            +Support_Files.In_Libexec_Dir ("libexec/valgrind/"));
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
        (Command => +Drrun, Native => True, others => <>);
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
        (Command => +"../libexec/gnatcoverage/isys_drv",
         Native  => False,
         others  => <>);
      System : constant String :=
        Context.Target_Family.all (Matches (1).First .. Matches (1).Last);
   begin
      Append_Arg (Result, System);
      Append_Arg (Result, Context.Exe_File.all);
      Append_Arg (Result, Trace_Input (Context));
      return Result;
   end ISystem;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      pragma Unreferenced (Matches);
      pragma Unreferenced (Context);

      Result : constant Command_Access := new Command_Type'
        (Command => +"",
         Native  => False,
         others  => <>);
   begin
      return Result;
   end Prepare;

   ----------------
   -- Visium_ELF --
   ----------------

   function Visium_ELF
     (Context : Context_Type; Matches : Match_Array) return Command_Access
   is
      pragma Unreferenced (Matches);

      Result : constant Command_Access := new Command_Type'
        (Command => +"visium-elf-run",
         Native  => False,
         others  => <>);
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
      Result.Native := False;

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

end Rundrv.Handlers;
