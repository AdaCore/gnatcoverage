------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;

with Outputs; use Outputs;

package body Inputs is

   ---------------
   -- Add_Input --
   ---------------

   procedure Add_Input (Inputs : in out Inputs_Type; Name : String) is

      procedure Add_Input (Name : String);
      --  Add_Input for this particular Inputs (the one given in parameter).

      ---------------
      -- Add_Input --
      ---------------

      procedure Add_Input (Name : String) is
      begin
         Inputs.Add_Input (Name);
      end Add_Input;

   --  Start of processing for Add_Input

   begin
      if Name'Length = 0 then
         return;
      end if;

      if Name (Name'First) /= '@' then
         Inputs.Append (new String'(Name));
      else
         if Name'Length = 1 then
            return;
         end if;

         if Name (Name'First + 1) /= '@' then
            declare
               File_Name : constant String :=
                 Name (Name'First + 1 .. Name'Last);
            begin
               Read_List_From_File (File_Name, Add_Input'Access);
            exception
               when Name_Error | Status_Error =>
                  Fatal_Error ("cannot open input list: " & File_Name);
            end;
         else
            Inputs.Append
              (new String'(Name (Name'First + 1 .. Name'Last)));
         end if;
      end if;
   end Add_Input;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Inputs  : in out Inputs_Type;
      Process : not null access procedure (Input : String))
   is
      use Input_Lists;

      procedure Input_Lists_Process (Position : Cursor);
      --  Call Process with Input at Cursor

      -------------------------
      -- Input_Lists_Process --
      -------------------------

      procedure Input_Lists_Process (Position : Cursor) is
         Input : constant String_Access := Element (Position);
      begin
         Process (Input.all);
      end Input_Lists_Process;

   --  Start of processing for Iterate

   begin
      Inputs.Iterate (Input_Lists_Process'Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length  (Inputs : Inputs_Type) return Ada.Containers.Count_Type is
   begin
      return Input_Lists.Length (Input_Lists.List (Inputs));
   end Length;

   -------------------------
   -- Read_List_From_File --
   -------------------------

   procedure Read_List_From_File
     (File_Name : String;
      Process   : not null access procedure (Name : String))
   is
      function Get_Line_Compatible (F : File_Type) return String;
      --  Same as Get_Line but eat trailing ASCII.CR so that DOS text files can
      --  be read easily.

      -------------------------
      -- Get_Line_Compatible --
      -------------------------

      function Get_Line_Compatible (F : File_Type) return String is
         L : constant String := Get_Line (F);
      begin
         if L'Length > 1 and then L (L'Last) = ASCII.CR then
            return L (L'First .. L'Last - 1);
         else
            return L;
         end if;
      end Get_Line_Compatible;

      F : File_Type;

   --  Start of processing for Read_List_From_File

   begin
      Open (F, In_File, File_Name);
      while not End_Of_File (F) loop
         declare
            L : constant String := Get_Line_Compatible (F);
         begin
            if L = "" or else L (L'First) = '#' then
               null;
            else
               Process (L);
            end if;
         end;
      end loop;
      Close (F);
   end Read_List_From_File;

end Inputs;
