------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

--  This stub embeds no instrumenter, which says nothing about whether gnatcov
--  supports C and C++: all programs but gnatcov64 delegate the C/C++
--  instrumentation to gnatcov64. Set_Builtin_Support is the sole authority on
--  that question.

package body Instrument.C is

   ---------------------------
   -- Clang_Target_Switches --
   ---------------------------

   function Clang_Target_Switches
     (Lang             : C_Family_Language;
      Compiler_Driver  : String;
      Compiler_Options : String_Vectors.Vector;
      Output_Dir       : String) return String_Vectors.Vector
   is
      pragma
        Unreferenced (Lang, Compiler_Driver, Compiler_Options, Output_Dir);
   begin
      return String_Vectors.Empty_Vector;
   end Clang_Target_Switches;

end Instrument.C;
