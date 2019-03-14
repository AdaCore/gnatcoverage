------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  Support for source instrumentation

with Inputs;

package Instrument is

   ---------------------------------------------
   -- Mapping of coverage buffer bits to SCOs --
   ---------------------------------------------

   --  As instrumentation is inserted, bit positions in coverage buffers are
   --  allocated, and these allocations are associated to low-level SCO Ids.
   --  Once low-level SCOs are converted to high-level SCOs, new mappings
   --  are generated to allow mapping bit positions to high level SCOs when
   --  processing buffers from a target run.

   type Any_Bit_Id is new Integer;
   No_Bit_Id : constant Any_Bit_Id := -1;
   subtype Bit_Id is Any_Bit_Id range 0 .. Any_Bit_Id'Last;

   procedure Instrument_Units_Of_Interest
     (Checkpoint_Filename : String;
      Units_Inputs        : Inputs.Inputs_Type;
      Auto_Dump_Buffers   : Boolean);
   --  Generate instrumented sources for the source files of all units of
   --  interest. Also save mappings between coverage buffers and SCOs to
   --  Checkpoint_Filename.
   --
   --  Units of interest are computed from the loaded project (-P/--projects),
   --  unless Units_Inputs is not empty: in this case, use the given unit names
   --  as the list of units of interest.
   --
   --  If Auto_Dump_Buffers is true, append a call to
   --  System.GNATcov.Traces.Output.Write_Trace_File for list of coverage
   --  buffers in all mains in the project.

end Instrument;
