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

with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;

with Switches; use Switches;

package body Calendar_Utils is

   function Image (Date : Time) return String is

      procedure Put_Pad (Num : Natural; S : out String);
      --  Return the string representation of a duration with as many leading
      --  zeros as required by the length of S.

      -------------
      -- Put_Pad --
      -------------

      procedure Put_Pad (Num : Natural; S : out String)
      is
         V : Natural := Num;
      begin
         for I in reverse S'Range loop
            S (I) := Character'Val ((V rem 10)
                                    + Character'Pos ('0'));
            V := V / 10;
         end loop;
      end Put_Pad;

   begin
      if Use_Local_Time then
         declare
            Time_Zone      : constant Time_Offset := Local_Time_Offset;
            Time_Zone_Sign : constant String :=
              (if Time_Zone >= 0 then "+" else "-");
            --  Even if Local_Time_Offset is 0, we will put a + sign to avoid
            --  ambiguities.

            Serialized_Offset : String (1 .. 6) := Time_Zone_Sign & "HH:MM";
            --  Time_Offset can be more than a day (up to 28 hours, according
            --  to the type specification), but we will always express it in
            --  hours and minutes for clarity purposes.

            Hours   : constant Natural :=
              Natural (abs Time_Zone) / 60;
            Minutes : constant Natural :=
              Natural (abs Time_Zone) mod 60;
         begin
            Put_Pad (Hours, Serialized_Offset (2 .. 3));
            Put_Pad (Minutes, Serialized_Offset (5 .. 6));
            return Local_Image (Date) & " " & Serialized_Offset;
         end;

      else
         return Ada.Calendar.Formatting.Image (Date) & " UTC";
      end if;
   end Image;

end Calendar_Utils;
