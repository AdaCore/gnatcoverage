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

with Traces_Elf;     use Traces_Elf;
with Traces_Dbase;   use Traces_Dbase;
with Traces_Lines;   use Traces_Lines;
with Traces_Stats;   use Traces_Stats;
with Types;          use Types;

package Traces_Sources is

   procedure New_Line
     (File  : Source_File_Index;
      Line  : Natural);
   --  Add File:Line to the subset of lines considered by the coverage
   --  operation. Initialize it to a no-code line.

   procedure Set_Line_State
     (File  : Source_File_Index;
      Line  : Natural;
      State : Line_State);
   --  Set coverage state of File:Line to State

   function Get_Line_State
     (File : Source_File_Index;
      Line : Natural) return Line_State;
   --  Return the current coverage state of File:Line

private

   Global_Stats : Stat_Array := (others => 0);
   --  Stats associated with the whole set of source files that this package
   --  considers (i.e. total numbers of lines, of partially covered /
   --  not covered / fully covered lines...)

end Traces_Sources;
