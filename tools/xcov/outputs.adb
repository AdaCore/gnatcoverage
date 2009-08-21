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

with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Strings;          use Strings;

package body Outputs is

   Report_Output_Dir : String_Acc := null;
   --  Name of the output directory. The reports will be generated
   --  in this directory.

   function Get_Output_Dir return String;
   --  Return the output dir. If Report_Output_Dir is null, initialize it
   --  to current dir.

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
   end Create_Output_File;

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      Put_Line (Standard_Error, Command_Name & ": " & Msg);
      Set_Exit_Status (Failure);
   end Error;

   ------------------
   --  Fatal_Error --
   ------------------

   procedure Fatal_Error (Msg : String) is
   begin
      Error (Msg);
      raise Fatal_Error_Exc;
   end Fatal_Error;

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

   ---------------------
   --  Set_Output_Dir --
   ---------------------

   procedure Set_Output_Dir (Output_Dir : String) is
   begin
      pragma Assert (Report_Output_Dir = null);

      if not Is_Directory (Output_Dir) then
         Fatal_Error ("bad value for output directory:" & ASCII.LF &
                      Output_Dir & " does not exists or is not a directory.");
      end if;

      Report_Output_Dir := new String'(Output_Dir);
   end Set_Output_Dir;

end Outputs;
