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

--  Support for computing some metrics (typically, percentage of coverage)
--  for a set of line (typically, a source file or a procedure).

with Traces_Lines; use Traces_Lines;

package Traces_Stats is

   type Stat_Array is array (Any_Line_State) of Natural;
   --  Array of stats indexed by the state of a line; it records
   --  the number of occurrences of each state in a set of lines.

   function Get_Stat_String (Stats : Stat_Array) return String;
   --  Return a String image of Stats

   function Get_Total (Stats : Stat_Array) return Natural;
   --  Return total line count, excluding No_Code lines

   function Ratio (Part : Natural; Total : Natural) return Natural;
   --  Total and Part being a number of lines, compute the ratio of the two
   --  quantities (Part / Total) and return this value as a percentage.

end Traces_Stats;
