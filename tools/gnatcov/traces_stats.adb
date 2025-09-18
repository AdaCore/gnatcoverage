------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Strings; use Strings;

package body Traces_Stats is

   use all type Unbounded_String;

   ---------------
   -- Get_Total --
   ---------------

   function Get_Total (Stats : Counter_Array) return Natural is
      Total : Natural := 0;
   begin
      for J in Stats'Range loop
         if J /= No_Code then
            Total := Total + Stats (J);
         end if;
      end loop;
      return Total;
   end Get_Total;

   --------------------------
   -- Get_Line_Stat_String --
   --------------------------

   function Get_Line_Stat_String (Stats : Li_Stat_Array) return String is
      Total : constant Natural := Get_Total (Stats);
   begin
      if Total = 0 then
         return "no code";
      else
         declare
            Res : constant String :=
              Natural'Image (Ratio (Stats (Covered), Total))
              & "% of"
              & Natural'Image (Total)
              & " lines covered";
         begin
            return Res (Res'First + 1 .. Res'Last);
         end;
      end if;
   end Get_Line_Stat_String;

   ---------------------------------
   -- Get_Obligation_Stats_String --
   ---------------------------------

   function Get_Obligation_Stats_String (Stats : Ob_Stat_Array) return String
   is
      SCO_Kind_Repr : constant array (Coverage_Level) of Unbounded_String :=
        (Stmt     => +"statement",
         Decision => +"decision",
         MCDC     => +"MC/DC",
         UC_MCDC  => +"UC MC/DC",
         ATC      => +"ATC",
         ATCC     => +"ATCC",
         others   => +"");
      Res           : Unbounded_String := Null_Unbounded_String;

      function Trimmed_Image (N : Natural) return String
      is (Trim (Natural'Image (N), Ada.Strings.Left));

   begin
      if Stats (Stmt).Total = 0 then
         return "no code";
      else
         for Level in Stats'Range loop
            if Stats (Level).Total /= 0 then
               declare
                  N_Covered : constant Natural :=
                    Stats (Level).Stats (Covered);
                  Total     : constant Natural := Stats (Level).Total;
                  R         : constant String :=
                    Trimmed_Image (Ratio (N_Covered, Total));
               begin
                  Res :=
                    Res
                    & R
                    & "% "
                    & SCO_Kind_Repr (Level)
                    & " coverage ("
                    & Trimmed_Image (N_Covered)
                    & " out of "
                    & Trimmed_Image (Total)
                    & ")"
                    & ASCII.LF;
               end;
            end if;
         end loop;
         return +Res;
      end if;
   end Get_Obligation_Stats_String;

   -----------
   -- Ratio --
   -----------

   function Ratio (Part : Natural; Total : Natural) return Natural is
      Percentage : Natural :=
        Natural (Float'Rounding (Float (Part) * 100.0 / Float (Total)));
   begin
      --  Percentages will be a gross approximation on the extremes, e.g.
      --  1 line not covered out of 201+ lines will still result in 1% of non
      --  covered lines.

      if Percentage = 100 and then Part /= Total then
         Percentage := 99;
      end if;

      if Percentage = 0 and then Part /= 0 then
         Percentage := 1;
      end if;

      return Percentage;

   end Ratio;

end Traces_Stats;
