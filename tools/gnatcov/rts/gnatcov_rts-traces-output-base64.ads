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

with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;
with GNATcov_RTS.Types;         use GNATcov_RTS.Types;

package GNATcov_RTS.Traces.Output.Base64 is

   pragma Preelaborate;

   procedure Write_Trace_File
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Program_Name   : String;
      Exec_Date      : Unsigned_64;
      User_Data      : String := "");

   procedure Write_Trace_File_Wrapper
     (Buffers_Groups : Coverage_Buffers_Group_Array;
      Program_Name   : String;
      Exec_Date      : Unsigned_64;
      User_Data      : String := "")
   renames Write_Trace_File;
   --  Renaming to mirror the API in GNATcov_RTS.Traces.Output.Files. This
   --  avoids special cases in the generation of automatic buffer dumps: except
   --  for the package that is WITHed, generated code is the same for
   --  --dump-channel=bin-file and for base64-stdout.

end GNATcov_RTS.Traces.Output.Base64;
