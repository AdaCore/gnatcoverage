------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2016-2024, AdaCore                     --
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
with Paths;         use Paths;
with Project;
with Support_Files;
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
         Paths.Build_Filename (Support_Files.Libexec_Dir, Where);
      Bundled : constant String :=
         Paths.Build_Filename (Dir, What);
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

   procedure Native_Linux
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      pragma Unreferenced (Matches);

      Cmd_Name : constant String :=
        Bundled_Or_Plain (What => "valgrind", Where => "bin");
   begin
      Native := True;
      Cmd := (Command => +Cmd_Name, others => <>);
      if not Rundrv_Trace.Is_Active then
         Append_Arg (Cmd, "--quiet");
      end if;
      Append_Arg (Cmd, "--tool=coverage");
      Append_Arg (Cmd, "--cov-exec-file=" & Trace_Input (Context));
      Append_Arg (Cmd, Context.Exe_File.all);

      --  If we're using our bundled-in valgrind, adjust VALGRIND_LIB
      --  accordingly.

      if Cmd_Name /= "valgrind" then
         Cmd.Environment.Insert
           (+"VALGRIND_LIB",
            +Support_Files.In_Libexec_Dir ("libexec/valgrind/"));
      end if;
   end Native_Linux;

   --------------------
   -- Native_Windows --
   --------------------

   procedure Native_Windows
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      pragma Unreferenced (Matches);

      Drrun    : constant String :=
         Bundled_Or_Plain (What => "drrun.exe", Where => "bin" & Bits);
      Drclient : constant String :=
         Bundled_Or_Plain (What => "qtrace.dll", Where => "lib" & Bits);
   begin
      Native := True;
      Cmd := (Command => +Drrun, others => <>);

      --  -quiet silences the warnings emitted by DynamoRIO on the assumption
      --  that it is invoked from an official release install tree.

      if not Rundrv_Trace.Is_Active then
         Append_Arg (Cmd, "-quiet");
      end if;
      Append_Arg (Cmd, "-no_follow_children");
      Append_Arg (Cmd, "-c", Drclient);
      Append_Arg (Cmd, "-o", Trace_Input (Context));
      Append_Arg (Cmd, "--", Context.Exe_File.all);
   end Native_Windows;

   -------------
   -- ISystem --
   -------------

   procedure ISystem
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      System : constant String :=
        Context.Target_Family.all (Matches (1).First .. Matches (1).Last);
   begin
      Native := False;
      Cmd := (Command => +"../libexec/gnatcoverage/isys_drv", others  => <>);
      Append_Arg (Cmd, System);
      Append_Arg (Cmd, Context.Exe_File.all);
      Append_Arg (Cmd, Trace_Input (Context));
   end ISystem;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      pragma Unreferenced (Context, Matches);
   begin
      Native := False;
      Cmd := (Command => +"", others  => <>);
   end Prepare;

   ----------------
   -- Visium_ELF --
   ----------------

   procedure Visium_ELF
     (Context : Context_Type;
      Matches : Match_Array;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      pragma Unreferenced (Matches);
   begin
      Native := False;
      Cmd := (Command => +"visium-elf-run", others  => <>);
      Append_Arg (Cmd, "--trace=" & Context.Trace_File.all);
      Append_Arg (Cmd, Context.Exe_File.all);
   end Visium_ELF;

   ------------------------
   -- Get_Gnatemu_Driver --
   ------------------------

   procedure Get_Gnatemu_Driver
     (Context : Context_Type;
      Found   : out Boolean;
      Cmd     : out Command_Type;
      Native  : out Boolean)
   is
      Gnatemu : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path
        (Context.Target_Family.all & "-gnatemu");
   begin
      if Gnatemu = null then
         Found := False;
         return;
      end if;

      Found := True;
      Native := False;
      Cmd := (Command => +Gnatemu.all, others => <>);
      Free (Gnatemu);

      --  Compute the subsets of options we need to pass.  As common options,
      --  we always need to pass the executable name to Gnatemu (last), and
      --  request the production of traces straight to the underlying emulator
      --  in addition to our own -eargs. Then we have the possible --kernel
      --  and --board extensions, and the project file related options.

      if Context.Kernel /= null then
         Append_Arg (Cmd, "--kernel=" & Context.Kernel.all);
      end if;

      if Context.Target_Board /= null then
         Append_Arg (Cmd, "--board=" & Context.Target_Board.all);
      end if;

      if Project.Is_Project_Loaded then
         Append_Arg
           (Cmd, "-P", String (Project.Project.Root_Project.Path_Name.Value));

         declare
            use Key_Element_Maps;
         begin
            for Scenario_Var in S_Variables.Iterate loop
               Append_Arg (Cmd, "-X" & Key (Scenario_Var)
                                & "=" & Element (Scenario_Var));
            end loop;
         end;
      end if;

      Append_Arg (Cmd, "--eargs");
      Append_Arg (Cmd, "-exec-trace", Trace_Input (Context));
      for Earg of Context.Eargs.all loop
         Append_Arg (Cmd, Earg.all);
      end loop;
      Append_Arg (Cmd, "--eargs-end");
      Append_Arg (Cmd, Context.Exe_File.all);
   end Get_Gnatemu_Driver;

end Rundrv.Handlers;
