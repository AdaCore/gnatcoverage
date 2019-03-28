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

--  Source trace files decoding

with Interfaces;

with GNATCOLL.Projects;

with Traces_Files;

package Instrument.Input_Traces is

   type Hash_Type is new Interfaces.Unsigned_32;

   type Coverage_Buffer is array (Bit_Id range <>) of Boolean with Pack;
   --  Content of a coverage buffer

   generic
      with procedure On_Trace_Entry
        (Closure_Hash    : Hash_Type;
         Unit_Name       : String;
         Unit_Part       : GNATCOLL.Projects.Unit_Parts;
         Stmt_Buffer     : Coverage_Buffer;
         Decision_Buffer : Coverage_Buffer;
         MCDC_Buffer     : Coverage_Buffer) is <>;
   procedure Generic_Read_Source_Trace_File
     (Filename : String;
      Result   : out Traces_Files.Read_Result);
   --  Read the given Filename source trace file and call On_Trace_Entry on
   --  each decoded trace entry.
   --
   --  If successful, Result.Success is set to True. Otherwise, Result is set
   --  to the corresponding error information.

   procedure Dump_Source_Trace_File (Filename : String);
   --  Read the given Filename source trace file and dump its content on the
   --  standard output. Emit a fatal error if the file is invalid.

end Instrument.Input_Traces;
