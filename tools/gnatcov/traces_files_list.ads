------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;

with GNAT.Strings; use GNAT.Strings;

with Checkpoints;
with Coverage;
with Traces_Files; use Traces_Files;

--  The list of all processed trace files

package Traces_Files_List is

   type Trace_File_Element (Kind : Trace_File_Kind := Trace_File_Kind'First)
   is record
      Context : String_Access;
      --  Null if this trace file was loaded by this instance of gnatcov.
      --  Otherwise, contains a string, to be decoded with
      --  Coverage.From_String, that describes the context where it has
      --  actually been processed.

      Filename : String_Access;
      --  File name for the trace file, as passed to "gnatcov coverage"

      case Kind is
         when Binary_Trace_File =>
            Trace : Trace_File_Type;
            --  In-memory data loaded from the trace file

         when Source_Trace_File =>
            null;
      end case;
   end record;

   type Trace_File_Element_Acc is access Trace_File_Element;

   package Traces_Files_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Trace_File_Element_Acc);

   Files : Traces_Files_Lists.List;

   procedure Checkpoint_Save
     (CSS     : access Checkpoints.Checkpoint_Save_State;
      Context : access Coverage.Context);
   --  Save the current list of trace files to S

   procedure Checkpoint_Load (CLS : access Checkpoints.Checkpoint_Load_State);
   --  Load list of trace files from S

end Traces_Files_List;
