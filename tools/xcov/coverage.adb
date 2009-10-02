------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Coverage is

   Current_Level : Coverage_Level := Unknown;
   --  Global variable that records the coverage operation that has been asked
   --  to xcov. This should be modified only one time by Set_Coverage_Level.

   -------------------------------
   -- All_Known_Coverage_Levels --
   -------------------------------

   function All_Known_Coverage_Levels return String is

      function List_Levels
        (Head : String; Tail : Coverage_Level) return String;
      --  Tail-recursive function to compute list

      -----------------
      -- List_Levels --
      -----------------

      function List_Levels
        (Head : String; Tail : Coverage_Level) return String
      is
         S : constant String := Head & To_Lower (Tail'Img);
      begin
         if Tail = Known_Coverage_Level'Last then
            return S;
         end if;
         return List_Levels (S & "|", Known_Coverage_Level'Succ (Tail));
      end List_Levels;

   --  Start of processing for All_Knwon_Coverage_Levels

   begin
      return List_Levels ("", Known_Coverage_Level'First);
   end All_Known_Coverage_Levels;

   --------------------------
   -- Dump_Coverage_Option --
   --------------------------

   procedure Dump_Coverage_Option (Report : File_Access) is
   begin
      Put_Line (Report.all,
                "Coverage level: " & To_Coverage_Option (Current_Level));
   end Dump_Coverage_Option;

   ------------------------
   -- Get_Coverage_Level --
   ------------------------

   function Get_Coverage_Level return Coverage_Level is
   begin
      return Current_Level;
   end Get_Coverage_Level;

   ------------------------
   -- Set_Coverage_Level --
   ------------------------

   procedure Set_Coverage_Level (Level : Coverage_Level) is
   begin
      pragma Assert (Current_Level = Unknown);
      Current_Level := Level;
   end Set_Coverage_Level;

   -----------------------
   -- To_Coverage_Level --
   -----------------------

   function To_Coverage_Level (Option : String) return Coverage_Level is
   begin
      return Coverage_Level'Value (Option);
   exception
      when Constraint_Error =>
         return Unknown;
   end To_Coverage_Level;

   ------------------------
   -- To_Coverage_Option --
   ------------------------

   function To_Coverage_Option (Level : Coverage_Level) return String is
   begin
      return Level'Img;
   end To_Coverage_Option;

end Coverage;
