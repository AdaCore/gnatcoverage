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
with Ada.Streams; use Ada.Streams;

with GNAT.Strings; use GNAT.Strings;

with Checkpoints;
with Coverage;
with Traces_Files; use Traces_Files;

--  The list of all processed trace files

package Traces_Files_List is

   type Trace_File_Element is record
      From_Checkpoint : Boolean;
      Filename        : String_Access;
      Trace           : Trace_File_Type;
   end record;

   type Trace_File_Element_Acc is access Trace_File_Element;

   package Traces_Files_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Trace_File_Element_Acc);

   Files : Traces_Files_Lists.List;

   procedure Checkpoint_Save
     (S       : access Root_Stream_Type'Class;
      Context : access Coverage.Context);
   --  Save the current list of trace files to S

   procedure Checkpoint_Load
     (S  : access Root_Stream_Type'Class;
      CS : access Checkpoints.Checkpoint_State);
   --  Load list of trace files from S

end Traces_Files_List;
