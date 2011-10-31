------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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

package body Traces_Stats is

   ---------------
   -- Get_Total --
   ---------------

   function Get_Total (Stats : Stat_Array) return Natural is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         if J /= No_Code then
            Total := Total + Stats (J);
         end if;
      end loop;
      return Total;
   end Get_Total;

   ---------------------
   -- Get_Stat_String --
   ---------------------

   function Get_Stat_String (Stats : Stat_Array) return String
   is
      Total : constant Natural := Get_Total (Stats);
   begin
      if Total = 0 then
         return "no code";

      else
         declare
            Res : constant String := Natural'Image (Ratio (Stats (Covered),
                                                           Total))
              & "% of" & Natural'Image (Total) & " lines covered";
         begin
            return Res (Res'First + 1 .. Res'Last);
         end;
      end if;
   end Get_Stat_String;

   -----------
   -- Ratio --
   -----------

   function Ratio (Part : Natural; Total : Natural) return Natural is
   begin
      return Natural (Float'Rounding (Float (Part) * 100.0
                                      / Float (Total)));
   end Ratio;

end Traces_Stats;
