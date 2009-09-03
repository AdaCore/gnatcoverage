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

with Traces_Elf;   use Traces_Elf;
with Traces_Dbase; use Traces_Dbase;
with Traces_Stats;   use Traces_Stats;
with Traces_Lines;   use Traces_Lines;
with Sources; use Sources;
with Types;   use Types;

with GNAT.Dynamic_Tables;
--  ??? This should be replaced by a dependancy on Ada.Containers.Vectors
--  when moved to sources.adb

package File_Tables is
   --  This package manages a source file table and, for each file,
   --  a table of its source lines. Coverage information can be
   --  associated with each file/line. Only object coverage is supported.

   --  ??? This package should disappear at some point. The handling of
   --  the file table should be moved to Sources. And the coverage-specific
   --  stuff to Traces_Sources.

   procedure Add_Line
     (File : Source_File_Index;
      Line : Natural;
      Info : Addresses_Info_Acc;
      Base : Traces_Base_Acc;
      Exec : Exe_File_Acc);
   --  Add File:Line to set of known source lines, if it doesn't exist already.
   --  Record the association of File:File with the given associated object
   --  code.
   --  ??? Find a way to make this independant from the attached coverage
   --  information.

   procedure New_Source_File (File : Source_File_Index);
   --  Initialize entry for File in source files table

   type Line_Chain;
   type Line_Chain_Acc is access Line_Chain;

   type Object_Coverage_Info is record
      --  This records maps an instruction set to its coverage information.
      --  An instruction set being a set of addresses in a given executable,
      --  this mapping can be built from a Traces_Base (that maps addresses to
      --  coverage information) and an Exe_File (that maps addresses
      --  to instructions).

      Instruction_Set : Addresses_Info_Acc;
      --  Range of instruction handled by this record

      Base : Traces_Base_Acc;
      --  Object coverage information for this instruction set

      Exec : Exe_File_Acc;
      --  Exec from where the address range has been extracted
   end record;

   type Line_Chain is record
      OCI  : Object_Coverage_Info;
      --  Object coverage information associated with an instruction set
      --  for this source line

      Next : Line_Chain_Acc;
      --  Next element in Line_Chain
   end record;

   type Line_Info is record
      --  Coverage information associated with a source line

      State : Line_State;
      --  Coverage state

      First_Line, Last_Line : Line_Chain_Acc;
      --  Detailled coverage information for this line
   end record;

   type Line_Info_Access is access Line_Info;

   type Source_Lines is private;

   procedure Iterate
     (Lines   : Source_Lines;
      Process : not null access procedure (Index : Natural));

   function Element
     (Lines : Source_Lines;
      Index : Natural)
     return Line_Info_Access;

   type File_Info is record
      --  Source file information.

      Lines      : Source_Lines;
      --  Source file to display in the reports.

      Stats      : Stat_Array := (others => 0);
      --  Counters associated with the file (e.g total number of lines, number
      --  of lines that are covered).

      To_Display : Boolean := False;
      --  If True, this file should be displayed in the reports. If False,
      --  it should be skipped.
      --  The global source table in sources.adb and the table of sources
      --  that are concerned by the current coverage operation
      --  (Source_Line_Tables) share the sames indices. However, the
      --  latter is a subset of the former. A non-contiguous subset, in the
      --  general case. So, To_Display is used to discriminate files that are
      --  really concerned by the coverage from files that just happens to
      --  have an index between two "real" elements.
      --
      --  ??? This should be removable at some point. Either we have
      --  some coverage information for this file, or we don't...
   end record;

   type File_Info_Access is access File_Info;

   procedure File_Table_Iterate
     (Process : not null access procedure (Index : Source_File_Index));

   function File_Table_Element
     (Index : Source_File_Index)
     return File_Info_Access;

private
   --  Describe a source file - one element per line.

   package Source_Line_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Line_Info_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 16,
      Table_Increment      => 100);

   type Source_Lines is new Source_Line_Tables.Instance;

end File_Tables;
