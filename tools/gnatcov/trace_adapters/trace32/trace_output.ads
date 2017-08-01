------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2017, AdaCore                       --
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

private with Ada.Containers.Ordered_Sets;

private with Interfaces;

with Traces;       use Traces;
with Traces_Files;
with Traces_Dbase;

package Trace_Output is

   type QEMU_Trace_Output is tagged limited private;

   procedure Open (This              : in out QEMU_Trace_Output;
                   Output_Trace_Path : String;
                   Decision_Map_Path : String := "")
     with Pre  => not Is_Open (This),
          Post => Is_Open (This);
   --  Open a flat trace file for output. An optional decision map can be
   --  loaded.

   function Is_Open (This : in out QEMU_Trace_Output) return Boolean;
   --  Return true if the trace file is open

   procedure Push_Entry (This        : in out QEMU_Trace_Output;
                         Trace_Entry : Traces.Trace_Entry)
     with Pre => Is_Open (This);
   --  Call this procedure every time an entry is executed. This procedure
   --  takes care of recording/updating the entry, it also handles entry
   --  history when necessary.

   procedure Close_Trace_File (This : in out QEMU_Trace_Output)
     with Pre  => Is_Open (This),
          Post => not Is_Open (This);
   --  Write remaining trace entries and close the trace file

private

   package Address_Set is
     new Ada.Containers.Ordered_Sets (Element_Type => Traces.Pc_Type,
                                      "<" => Interfaces."<",
                                      "=" => Interfaces."=");

   type QEMU_Trace_Output is tagged limited record
      Open         : Boolean := False;
      Desc         : Traces_Files.Trace_File_Descriptor;
      Trace_File   : Traces_Files.Trace_File_Type;
      Base         : Traces_Dbase.Traces_Base;
      Decision_Map : Address_Set.Set;
   end record;

   procedure Update_Entry (This        : in out QEMU_Trace_Output;
                           Trace_Entry : Traces.Trace_Entry);
   --  Update flags of an existing entry or add a new entry to the database

   procedure Load_Decision_Map
     (This              : in out QEMU_Trace_Output;
      Decision_Map_Path : String);
   --  Open decision map file, load addresses in a database and close the file.
   --  If the decision map cannot be loaded, it is not considered as an error
   --  state.

   function Keep_History (This : in out QEMU_Trace_Output;
                          Pc   : Pc_Type) return Boolean;
   --  Return true if execution history should be kept for this address

end Trace_Output;
