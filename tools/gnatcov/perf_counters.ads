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

--  Performance counters

package Perf_Counters is

   type Any_Counter_Type is
      --  Counters maintained during execution

     (Addr_Map_Cache_Hit,
      Addr_Map_Cache_Miss,
      Line_Table_Alloc,
      Line_Table_Alloc_Size,

      --  Counters computed at end of run, just before displaying results

      Line_Table_Alloc_Avg_Size);

   subtype Counter_Type is Any_Counter_Type
     range Any_Counter_Type'First .. Line_Table_Alloc_Size;

   procedure Bump (C : Counter_Type; How_Many : Natural := 1);
   pragma Inline (Bump);
   --  Increment counter C by the specified amount

   procedure Display;
   --  Display every counter's value

end Perf_Counters;
