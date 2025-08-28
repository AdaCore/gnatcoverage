------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with Traces_Files; use Traces_Files;
with Strings;      use Strings;

package LLVM is

   procedure Create_LLVM_Temp_Dir (Auto_Delete : Boolean);

   function Make_LLVM_Checkpoint_From_Traces
     (Trace_Inputs : Requested_Trace_Vectors.Vector;
      Exe_File     : String) return Unbounded_String;
   --  Use `llvm-profdata` and the gnatcov LLVM trace adapter to convert the
   --  trace inputs into an LLVM JSON checkpoint.
   --  Return the expanded the absolute path of the JSON file.

end LLVM;
