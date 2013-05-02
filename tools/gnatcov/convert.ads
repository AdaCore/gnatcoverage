------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2013-2013, AdaCore                     --
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

--  This package provides data and routines to support the "convert"
--  command of gnatcov.

with GNAT.OS_Lib; use GNAT.OS_Lib;

package Convert is

   type Trace_Source_Type is (Unspecified, Isystem_5634);

   Trace_Source : Trace_Source_Type := Unspecified;

   procedure Set_Trace_Source (Arg : String);
   --  Set the trace source from the command line arg.

   HW_Trigger_Arg : String_Access;
   Input_Arg      : String_Access;

   procedure Run_Convert (Exe_Name : String_Access;
                          Output   : String_Access;
                          Histmap  : String_Access);

end Convert;
