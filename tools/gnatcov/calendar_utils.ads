------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Ada.Calendar; use Ada.Calendar;

package Calendar_Utils is

   type Any_Timezone is (Local_Time, UTC_Time);

   function Image (Date : Time) return String;
   --  Wrapper for Ada.Calendar.Formatting.Image. If --local-time option is on,
   --  return the date in the local timezone offset.

   function To_Timezone (Option : String) return Any_Timezone;
   --  Return the timezone designated by the option

end Calendar_Utils;
