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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Command_Line;
with Support_Files;
with Switches;

package body Outputs is

   Report_Output_Dir : String_Access := null;
   --  Name of the output directory. The reports will be generated
   --  in this directory.

   -------------------------
   --  Create_Output_File --
   -------------------------

   procedure Create_Output_File
     (File      : out File_Type;
      File_Name : String)
   is
      Full_Path_Name : constant String :=
        Get_Output_Dir
        & Directory_Separator
        & File_Name;
   begin
      Create (File, Out_File, Full_Path_Name);
   exception
      when E : Name_Error =>
         Error ("failed to create output file " & Full_Path_Name & ":");
         Error (Ada.Exceptions.Exception_Information (E));
         raise;
   end Create_Output_File;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Warning_Or_Error (Support_Files.Gnatcov_Command_Name & ": " & Msg);
      Set_Exit_Status (Failure);
   end Error;

   ------------------
   --  Fatal_Error --
   ------------------

   procedure Fatal_Error (Msg : String) is
   begin
      Error (Msg);
      raise Xcov_Exit_Exc;
   end Fatal_Error;

   ----------------------------
   -- Fatal_Error_With_Usage --
   ----------------------------

   procedure Fatal_Error_With_Usage (Msg : String) is
   begin
      Error (Msg);
      Command_Line.Parser.Print_Usage
        (Switches.Arg_Parser, False, True, Switches.Args.Command);
      raise Xcov_Exit_Exc;
   end Fatal_Error_With_Usage;

   ---------------------
   --  Get_Output_Dir --
   ---------------------

   function Get_Output_Dir return String is
   begin
      if Report_Output_Dir = null then
         Set_Output_Dir ("./");
      end if;

      return Report_Output_Dir.all;
   end Get_Output_Dir;

   -----------------
   -- Normal_Exit --
   -----------------

   procedure Normal_Exit is
   begin
      Set_Exit_Status (Success);
      raise Xcov_Exit_Exc;
   end Normal_Exit;

   ------------------------
   -- Output_Dir_Defined --
   ------------------------

   function Output_Dir_Defined return Boolean is (Report_Output_Dir /= null);

   ---------------------
   --  Set_Output_Dir --
   ---------------------

   procedure Set_Output_Dir (Output_Dir : String) is
   begin
      Free (Report_Output_Dir);

      if not Is_Directory (Output_Dir) then
         Put_Line (Standard_Error, "info: creating output path " & Output_Dir);
         begin
            Ada.Directories.Create_Path (Output_Dir);
         exception
            when Exc : Name_Error | Use_Error =>
               Fatal_Error
                 ("cannot create output path "
                  & Output_Dir
                  & ": " & Ada.Exceptions.Exception_Message (Exc));
         end;
      end if;

      Report_Output_Dir := new String'(Output_Dir);
   end Set_Output_Dir;

   ----------
   -- Warn --
   ----------

   procedure Warn (Msg : String) is
   begin
      Warning_Or_Error ("warning: " & Msg);
   end Warn;

   ----------------------
   -- Warning_Or_Error --
   ----------------------

   procedure Warning_Or_Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Msg);
   end Warning_Or_Error;

end Outputs;
