------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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

with Traces_Files; use Traces_Files;

package body Options_Files is

   function Is_Empty (Input : Command_Line_Input) return Boolean;

   function Is_Empty (Input : Command_Line_Input) return Boolean is
   begin
      return Input.Index = 0;
   end Is_Empty;

   function New_Command_Line_Input return Command_Line_Input
   is
      New_Input : Command_Line_Input;
   begin
      New_Input := new Command_Line_Input_Type;
      return New_Input;
   end New_Command_Line_Input;

   procedure Add_Input
     (Input      : in out Command_Line_Input;
      Trace_File : File_Name;
      Exec_File  : File_Name)
   is
      New_Input : Command_Line_Input;
   begin
      if Is_Empty (Input) then
         Input.Exec_File := Exec_File;
         Input.Trace_File := Trace_File;
         Input.Index := 1;
      else
         New_Input := New_Command_Line_Input;
         New_Input.Trace_File := Trace_File;
         New_Input.Exec_File := Exec_File;
         New_Input.Index := Input.Index + 1;
         New_Input.Next := Input;
         Input := New_Input;
      end if;
   end Add_Input;

   function Length (Input : Command_Line_Input) return Index_Entry is
   begin
      return Input.Index;
   end Length;

   procedure Open_Execs
     (Input : Command_Line_Input;
      Execs : in out Exec_Base_Type)
   is
      Cursor     : Command_Line_Input := Input;
   begin
      while Cursor /= null loop
         Insert_Exec (Execs, Cursor.Exec_File.all);
         Cursor := Cursor.Next;
      end loop;
   end Open_Execs;

   procedure Read_Traces
     (Input : Command_Line_Input;
      Base  : in out Traces_Base)
   is
      Cursor     : Command_Line_Input := Input;
      Trace_File : Trace_File_Type;
   begin
      while Cursor /= null loop
         Read_Trace_File (Cursor.Trace_File.all, Trace_File, Base);
         Free (Trace_File);
         Cursor := Cursor.Next;
      end loop;
   end Read_Traces;

end Options_Files;
