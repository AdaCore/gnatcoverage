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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

package body Perf_Counters is

   Enabled  : Boolean := False;
   Counters : array (Any_Counter_Type) of Natural := (others => 0);

   ------------
   -- Enable --
   ------------

   procedure Enable (Enable : Boolean := True) is
   begin
      Enabled := Enable;
   end Enable;

   ----------
   -- Bump --
   ----------

   procedure Bump (C : Counter_Type; How_Many : Natural := 1) is
   begin
      if Enabled then
         Counters (C) := Counters (C) + How_Many;
      end if;
   end Bump;

   -------------
   -- Display --
   -------------

   procedure Display is
      Max_Name, Max_Val : Natural := 0;
   begin
      --  Compute averages

      if Counters (Line_Table_Alloc) > 0 then
         Counters (Line_Table_Alloc_Avg_Size) :=
           Counters (Line_Table_Alloc_Size)
           / Counters (Line_Table_Alloc);
      end if;

      --  Compute maximum name and value lengths

      for J in Counters'Range loop
         Max_Name := Natural'Max (Max_Name, J'Img'Length);
         Max_Val  := Natural'Max (Max_Val,  Counters (J));
      end loop;

      --  Display

      New_Line;
      Put_Line ("Performance counters");
      Put_Line ("--------------------");
      New_Line;

      Max_Name := Max_Name + 1;
      Max_Val  := Max_Val'Img'Length;

      for J in Counters'Range loop
         Put_Line (Head (J'Img, Max_Name) & Tail (Counters (J)'Img, Max_Val));
      end loop;
   end Display;

end Perf_Counters;
