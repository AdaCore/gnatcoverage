------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

with Text_IO;                   use Text_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

--  This program  - isys_drive - is called by 'gnatcov run' when the target
--  specified to gnatcov is one that indicates that the program should be run
--  through an iSytem probe, on a target board connected to that probe.
--
--  Isys_drive:
--   - Creates a workspace directory under the current working
--     directory and populates it with configuration files needed
--     by winIdea (the program which runs the probe).
--   - Runs a python program which directs winIdea to run the executable
--     program on the board and generate program trace data in a file.
--   - Runs a translation program which converts the trace from winIdea
--     into the format used by gnatcov.
--
--  There are 3 arguments to this program. The first 2 designate the
--  target processor and the executable file to be run on the target. The
--  third argument is either the name of the trace file initialized by
--  gnatcov and appended to by the trace conversion program, or an argument
--  of the form 'histmap=HH,TT', where TT is the name of the trace file,
--  and HH is the name of a history file which is passed to the trace
--  conversion program to tell it to output 'history' traces and identifying
--  addresses for which history information is needed.
--
--  Their is some error checking done by this program, but
--  since it is intended to be called from gnatcov (no fat
--  fingers), many things are not checked.
--
--  This program should reside in ../libexec/gnatcov, relative
--  to the directory containing the gnatcov program. It expects
--  that other programs that it calls (the python program, and
--  the trace conversion program) to reside in the same directory.
--  It expcts to find the winIdea configuration files in
--  ../../share/gnatcov (relative to isys_drive itself).

procedure isys_drv is
   S_Idx1, S_Idx2 : Positive;

   Executable_Path : String_Access;
   Histfile_Path   : String_Access;
   Tracefile_Path  : String_Access;
   --  Pathnames for the executable to be run on the board,
   --  the history file and the trace file.

   Execs_Dir : String := Normalize_Pathname ("..", Command_Name);
   Share_Dir : String := Normalize_Pathname ("../../share/gnatcov", Execs_Dir);
   --  Paths to directories containing this program and other programs
   --  called by it, and to the directory containing winIdea configuration
   --  files.

   Python_P    : String_Access := Locate_Exec_On_Path ("python");
   Python_Args : Argument_List (1 .. 3);

   Trace_Conversion_Path : String :=
    Normalize_Pathname ("Nexus_Trace_Gen", Execs_Dir);
   Trace_Conv_Args : Argument_List (1 .. 4);

   Success, C_Success : Boolean;
   --  Used by various procedures to report/calculate success.

   Project_Files_Basename : String_Access;
   --  The names of the various winIdea configuration files which are copied
   --  from the share/gnatcov location to the workspace directoy are
   --  constructed from this string, whose value depends upon the particular
   --  processor.

   CWD : String := Get_Current_Dir;

   function Create_Workspace_Dir return String;
   --  Creates the Workspace directry and returns it's name.
   --  Returns "" if it cannot create the directory.

   function Create_Workspace_Dir return String is
      D_Name : String := CWD & "workspace";
      --  ??? The point of creating a function for this is to
      --  be able to find a name that does not collide. So this
      --  fixed name should go away at some point.
   begin
      Make_Dir (D_Name);
      return D_Name;
   exception
      when Directory_Error =>
         return "";
   end Create_Workspace_Dir;

   Workspace_Dir : String := Create_Workspace_Dir;

begin

   if Argument_Count /= 3 then
      Put_Line (Standard_Error, "Wrong Number of args to isys_drv.");
      OS_Exit (1);
   end if;

   Python_Args (1) :=
     new String'(Normalize_Pathname ("isys_trace.py", Execs_Dir));
   if Argument (1) = "5554" then
      Project_Files_Basename := new String'("min5554");
      Python_Args (2) := new String'("5554");
   elsif Argument (1) = "5634" then
      Project_Files_Basename := new String'("min5634");
      Python_Args (2) := new String'("5634");
   else
      Put_Line (Standard_Error, "Unknown processor id in call to isys_drv.");
      OS_Exit (1);
   end if;

   Executable_Path := new String'(Normalize_Pathname (Argument (2)));

   --  Handle the casses where the tracefile argument is either the
   --  name of the trace file created by gnatcov (with some information
   --  records, and to be appended with the trace records), or of the
   --  form histmap=H,T with H being the name of a history map file
   --  and T is the name of the trace file.
   --
   --  Here we create the path names of the files, or "" for the history
   --  map file in the first case above.

   S_Idx1 := Argument (3)'First;
   if Argument (3)'Length > 8 and then
     Argument (3) (S_Idx1 .. S_Idx1 + 7) = "histmap="
   then
      S_Idx1 := S_Idx1 + 8;
      S_Idx2 := S_Idx1;
      loop
         exit when S_Idx2 = Argument (3)'Last
           or else Argument (3) (S_Idx2) = ',';
         S_Idx2 := S_Idx2 + 1;
      end loop;
      if S_Idx2 = Argument (3)'Last then
         Put_Line (Standard_Error, "Missing tracefile arg to isys_drv.");
         OS_Exit (1);
      end if;
      Histfile_Path := new String'
        (Normalize_Pathname (Argument (3) (S_Idx1 .. S_Idx2 - 1)));
      Tracefile_Path := new String'
        (Normalize_Pathname (Argument (3) (S_Idx2 + 1 .. Argument (3)'Last)));
   else
      Histfile_Path := new String'("");
      Tracefile_Path := new String'(Argument (3));
   end if;

   if Python_P = null then
      Put_Line (Standard_Error, "python not found.");
      OS_Exit (1);
   end if;
   Python_Args (3) := new String'(Executable_Path.all);

   Copy_File (Share_Dir & '\' & Project_Files_Basename.all & ".xjrf",
              Workspace_Dir, C_Success);
   Copy_File (Share_Dir & '\' & Project_Files_Basename.all & ".xqrf",
              Workspace_Dir, Success);
   C_Success := C_Success and Success;
   Copy_File (Share_Dir & '\' & Project_Files_Basename.all & ".trd",
              Workspace_Dir, Success);
   C_Success := C_Success and Success;
   if not C_Success then
      Put_Line (Standard_Error, "Error copying winIdea config files.");
      OS_Exit (1);
   end if;
   --  The iSystem program winIdea is what interfaces the host computer
   --  to the iSystem HW probe which is connected to the target board.
   --  The python script which is called below directs winIdea to run
   --  the program, generating a Nexus trace file, and then write out
   --  that trace file. WinIdea gets configuration information from
   --  a "workspace" file (the .xjrf file) and other configuration
   --  files (the others above contain parameters specifying trace
   --  generation). WinIdea also uses the directory containing the
   --  workspace configuration file as the default location for other
   --  files it needs/produces (such as the Nexus trace file). The
   --  project file provided is minimal, containing lttle more than
   --  the type of hw (probe, target board and communication channel)
   --  being used.

   Spawn (Python_P.all, Python_Args, Success);
   if not Success then
      Put_Line (Standard_Error, "winIdea python script failed.");
      OS_Exit (1);
   end if;

   Trace_Conv_Args (1) := Executable_Path;
   Trace_Conv_Args (2) := new String'(Workspace_Dir & '\' & "nexus_trace.bin");
   Trace_Conv_Args (3) := Histfile_Path;
   --  ??? Empty string doesn't get through to trace conversion program.
   --  Compensating on the other end using arg count.
   Trace_Conv_Args (4) := Tracefile_Path;
   Spawn (Trace_Conversion_Path, Trace_Conv_Args, Success);
   if not Success then
      Put_Line (Standard_Error, "Error from trace conversion.");
      OS_Exit (1);
   end if;
   declare
   begin
      Remove_Dir (Workspace_Dir, Recursive => True);
   exception
         --  Remove_Dir was failing when calling itself recursively after
         --  removing the contents of the directory, but a simple use
         --  of Remove_Dir after that failure, sould succeed. Thus, this
         --  delay and try once more. Big improvement. ???
      when Directory_Error =>
         Put_Line ("Try remove_dir again.");
         delay 3.0;
         Remove_Dir (Workspace_Dir, Recursive => True);
   end;
   OS_Exit (0);
end isys_drv;
