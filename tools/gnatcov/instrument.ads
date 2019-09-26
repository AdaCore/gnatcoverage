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

with GNAT.Regexp;

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

   type Any_Dump_Method is (Manual, At_Exit, Main_End);
   --  Method to use in order to automatically dump coverage buffers in
   --  instrumented programs. See the user documentation for the --dump-method
   --  command-line option.

   subtype Auto_Dump_Method is Any_Dump_Method range At_Exit .. Main_End;

   type Any_Language_Version is (Ada_83, Ada_95, Ada_2005, Ada_2012);

   procedure Instrument_Units_Of_Interest
     (SID_Filename         : String;
      Dump_Method          : Any_Dump_Method;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp);
   --  Generate instrumented sources for the source files of all units of
   --  interest. Also save mappings between coverage buffers and SCOs to
   --  SID_Filename.
   --
   --  Depending on Dump_Method, instrument mains to schedule a call to
   --  System.GNATcov.Traces.Output.Write_Trace_File for list of coverage
   --  buffers in all mains in the project.
   --
   --  Language_Version restricts what source constructs the instrumenter is
   --  allowed to use. For instance, if Ada_2005 (or a lower version) is
   --  passed, it will not be allowed to introduce expression functions, and
   --  thus will emit a warning when it needed to do so.
   --
   --  If Ignored_Source_File is non-null, ignore files whose names match the
   --  accessed pattern.

end Instrument;
