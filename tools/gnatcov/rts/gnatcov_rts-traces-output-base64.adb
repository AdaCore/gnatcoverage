------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit needs to be compilable with Ada 95 compilers

with GNATcov_RTS.Strings; use GNATcov_RTS.Strings;

package body GNATcov_RTS.Traces.Output.Base64 is

   procedure Write_Trace_File_C
     (Buffers_Groups : GNATcov_RTS_Coverage_Buffers_Group_Array;
      Program_Name   : GNATcov_RTS_String;
      Exec_Date      : Unsigned_64;
      User_Data      : GNATcov_RTS_String);
   pragma
     Import (C, Write_Trace_File_C, "gnatcov_rts_write_trace_file_base64");

   ----------------------
   -- Write_Trace_File --
   ----------------------

   procedure Write_Trace_File
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Program_Name   : String;
      Exec_Date      : Unsigned_64;
      User_Data      : String := "") is
   begin
      --  See the note about -gnatw.X in gnatcov_rts.gpr

      pragma Warnings (Off);
      Write_Trace_File_C
        ((Buffers_Groups'Length, Buffers_Groups'Address),
         (Program_Name'Address, Program_Name'Length),
         Exec_Date,
         (User_Data'Address, User_Data'Length));
      pragma Warnings (On);
   end Write_Trace_File;

end GNATcov_RTS.Traces.Output.Base64;
