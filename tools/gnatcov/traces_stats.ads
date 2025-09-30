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

--  Support for computing some metrics (typically, percentage of coverage)
--  for a set of line (typically, a source file or a procedure).

with Coverage_Options; use Coverage_Options;
with Traces_Lines;     use Traces_Lines;

package Traces_Stats is

   type Counter_Array is array (Any_Line_State) of Natural;
   --  Array of stats indexed by the coverage state (covered, partially
   --  covered, etc.).

   subtype Li_Stat_Array is Counter_Array;
   --  Array of stats for a set of lines

   type SCO_Tally is record
      Total : Natural := 0;
      Stats : Counter_Array := (others => 0);
   end record;

   type Ob_Stat_Array is array (Source_Coverage_Level) of SCO_Tally;
   --  Array of stats for each coverage level obligation (stmt, decision ...),
   --  for a set of coverage obligations.

   function Get_Line_Stat_String (Stats : Li_Stat_Array) return String;
   --  Return a String image of line stats

   function Get_Obligation_Stats_String (Stats : Ob_Stat_Array) return String;
   --  Return a String image of obligation stats

   function Get_Total (Stats : Counter_Array) return Natural;
   --  Return total line count, excluding No_Code lines

   function Ratio (Part : Natural; Total : Natural) return Natural;
   --  Total and Part being a number of lines, compute the ratio of the two
   --  quantities (Part / Total) and return this value as a percentage.
   --
   --  This function arranges never to round towards 0 or 100 (%).
   --  This ensures that 100 or 0 are only returned for really complete
   --  coverage or absence thereof, that is, for Part == Total or Part = 0,
   --  respectively. This might incur inaccuracies when summing ratios, with
   --  sum /= 100, really minor against the representativeness of extreme
   --  values.

end Traces_Stats;
