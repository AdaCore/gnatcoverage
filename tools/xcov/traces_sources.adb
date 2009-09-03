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

with File_Tables; use File_Tables;

package body Traces_Sources is

   ---------------
   --  New_Line --
   ---------------

   procedure New_Line
     (File  : Source_File_Index;
      Line  : Natural)
   is
      FI : constant File_Info_Access := File_Table_Element (File);
      LI : constant Line_Info_Access := Element (FI.Lines, Line);
   begin
      LI.State := No_Code;
      FI.Stats (No_Code) := FI.Stats (No_Code) + 1;
      Global_Stats (No_Code) := Global_Stats (No_Code) + 1;
   end New_Line;

   --------------------
   -- Get_Line_State --
   --------------------

   function Get_Line_State
     (File : Source_File_Index;
      Line : Natural) return Line_State
   is
      FI : constant File_Info_Access := File_Table_Element (File);
      LI : constant Line_Info_Access := Element (FI.Lines, Line);
   begin
      return LI.State;
   end Get_Line_State;

   --------------------
   -- Set_Line_State --
   --------------------

   procedure Set_Line_State
     (File  : Source_File_Index;
      Line  : Natural;
      State : Line_State)
   is
      FI : constant File_Info_Access := File_Table_Element (File);
      LI : constant Line_Info_Access := Element (FI.Lines, Line);
   begin
      pragma Assert (State /= No_Code);
      FI.Stats (LI.State) := FI.Stats (LI.State) - 1;
      Global_Stats (LI.State) := Global_Stats (LI.State) - 1;
      LI.State := State;
      FI.Stats (LI.State) := FI.Stats (LI.State) + 1;
      Global_Stats (LI.State) := Global_Stats (LI.State) + 1;
   end Set_Line_State;

end Traces_Sources;
