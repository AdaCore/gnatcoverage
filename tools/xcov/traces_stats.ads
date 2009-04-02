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

--  Support for computing some metrics (typically, percentage of coverage)
--  for a set of line (typically, a source file or a procedure).

with Traces_Lines; use Traces_Lines;

package Traces_Stats is

   type Stat_Array is array (Line_State) of Natural;
   --  Array of stats indexed by the state of a line; it records
   --  the number of occurrences of each state in a set of lines.

   function Get_Stat_String (Stats : Stat_Array) return String;
   --  Return a String image of Stats

   type Counters is record
      --  For a set of lines, keep a record of the number of lines that
      --  have a given "state" (fully covered/partially covered) so that
      --  statistics can be built from these indications.

      Fully   : Natural;
      --  Number of lines fully covered according the coverage criteria

      Partial : Natural;
      --  Number of lines partially covered according the coverage criteria

      Total   : Natural;
      --  Total number of lines
   end record;

   function Get_Counters (Stats : Stat_Array) return Counters;
   --  From the number of occurences of each state in a set of line
   --  (in Stats), compute the number of lines fully covered, partially
   --  covered, and the total number of lines. Return the result.

end Traces_Stats;
