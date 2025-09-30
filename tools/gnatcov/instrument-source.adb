------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with Checkpoints;       use Checkpoints;
with Coverage;
with Files_Handling;    use Files_Handling;
with Instrument.Common; use Instrument.Common;
with Traces_Files;      use Traces_Files;

--  Implementation of the gnatcov instrument-source command, which instrument
--  the given command-line unit. If the unit is of a unit based language, the
--  unit name is the name of the unit, otherwise, it is the (full) filename.

procedure Instrument.Source
  (Unit_Name         : String;
   SID_Name          : String;
   Instrumenter      : in out Language_Instrumenter'Class;
   Files_Of_Interest : File_Sets.Set;
   Prj               : Prj_Desc)
is
   Context : aliased Coverage.Context := Coverage.Get_Context;
begin
   --  Even though instrumentation does not create any traces, the structure
   --  of a SID file is basically a checkpoint, so it has a Trace_Kind field
   --  in its header. Instead of leaving it to Unknown (default value) mark it
   --  as Source_Trace_File so that when the .sid file is loaded, it will set
   --  gnatcov in "source trace mode" and it will be rejected if binary traces
   --  have already been loaded.

   Update_Current_Trace_Kind (Source_Trace_File);

   --  Instrument all of the source files implementing the compilation unit.
   --  For Ada, this means instrumenting the body / spec / separates, and for
   --  C/C++, this means instrumenting the .c file and the included headers.

   Instrumenter.Instrument_Unit (Unit_Name, Prj, Files_Of_Interest);

   --  Save the SCOs for the unit in the SID file

   Checkpoints.Checkpoint_Save
     (SID_Name, Context'Access, Purpose => Checkpoints.Instrumentation);

   if SC_Obligations.SCOs_Trace.Is_Active then
      SC_Obligations.Dump_All_SCOs;
   end if;

   Checkpoints.Checkpoint_Clear;

end Instrument.Source;
