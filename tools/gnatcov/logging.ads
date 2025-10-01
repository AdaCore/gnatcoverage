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

with GNATCOLL.Traces;

with Strings; use Strings;

package Logging is

   subtype GNATCOLL_Trace is GNATCOLL.Traces.Logger;
   --  Convenience subtype so that trace creation requires "with Switches;"
   --  only instead of also "with GNATCOLL.Traces;".

   GNATCOLL_Trace_Prefix : constant String := "gnatcov.";
   --  Prefix to use for all GNATCOLL traces defined in GNATcov

   function Create_Trace (Unit_Name : String) return GNATCOLL_Trace;
   --  Wrapper around GNATCOLL.Traces.Create to create GNATcov-specific traces
   --  (with GNATCOLL_Trace_Prefix and standard settings).

   procedure Initialize (Verbose : Boolean; To_Enable : String_Vectors.Vector);
   --  Initialize GNATCOLL traces from the ".gnatdebug" config file, if any.
   --
   --  Then, if Verbose is True, make all GNATcov traces active. Otherwise,
   --  enable GNATcov traces whose name is included in To_Enable.

   procedure Get_Configuration
     (Verbose : out Boolean; To_Enable : out String_Vectors.Vector);
   --  Get the arguments that were passed to Initialize

   procedure Print_List;
   --  Print the list of GNATcov traces on the standard output

end Logging;
