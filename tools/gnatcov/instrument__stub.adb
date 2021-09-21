------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2021, AdaCore                     --
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

--  This is a stub version of the root Instrumentation package, which does not
--  bring a dependency to the Instrument.Ada and the Instrument.C units. This
--  allows us to no longer depend on libclang/libadalang for executables such
--  as gnatcov32 that don't need the instrumentation capabilities, greatly
--  reducing their size and compilation time.

package body Instrument is
   procedure Instrument_Units_Of_Interest
     (Dump_Config          : Any_Dump_Config;
      Language_Version     : Any_Language_Version;
      Ignored_Source_Files : access GNAT.Regexp.Regexp) is
   begin
      raise Program_Error with "Stub of Instrument_Units_Of_Interest."
                               & " Unreachable code.";
   end Instrument_Units_Of_Interest;
end Instrument;
