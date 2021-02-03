------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

--  This packages provides a set of routines to manage xcov's outputs
--  (error messages and annotated reports).

with Ada.Text_IO; use Ada.Text_IO;

package Outputs is

   ------------
   -- Errors --
   ------------

   Xcov_Exit_Exc : exception;
   --  Cause Xcov to terminate. Do not use in an explicit raise; call
   --  Fatal_Error or Normal_Exit instead.

   procedure Warn (Msg : String);
   --  Display Msg on standard error prefixed with "warning: "

   procedure Error (Msg : String);
   --  Display Msg on standard error and set exit status to failure

   procedure Warning_Or_Error (Msg : String);
   --  Display Msg on standard error

   procedure Fatal_Error (Msg : String);
   pragma No_Return (Fatal_Error);
   --  Same as Error, and cause Xcov to terminate by raising Fatal_Error_Exc

   procedure Fatal_Error_With_Usage (Msg : String);
   --  Same as Fatal_Error, but print the command-line usage before terminating

   procedure Normal_Exit;
   --  Cause Xcov to terminate. exit status OK

   -----------------------
   -- Output management --
   -----------------------

   function Output_Dir_Defined return Boolean;
   --  True if the output directory has been set

   procedure Set_Output_Dir (Output_Dir : String);
   --  Set the path to the directory where report files will be stored.
   --  This procedure shall be called before any use of Create_Output_File
   --  (presumably when parsing xcov's options). The default output dir is
   --  the root project's object directory if using a project, or the current
   --  directory if not.

   function Get_Output_Dir return String;
   --  Return the output dir. If Report_Output_Dir is null, initialize it
   --  to current dir.

   procedure Create_Output_File
     (File      : out File_Type;
      File_Name : String);
   --  Create a file whose name is File_Name and located in output dir, as
   --  specified by Set_Output_Dir; then open it for write access and
   --  return its file descriptor.
   --  If output dir has not been initialized by Set_Output_Dir, it is
   --  set to the current directory.

end Outputs;
