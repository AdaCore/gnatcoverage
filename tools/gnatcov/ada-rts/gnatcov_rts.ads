------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

--  Namespace for all support packages required to do instrumentation-based
--  coverage computation in GNATcoverage.

--  This unit needs to be compilable with Ada 95 compilers

with System;

package GNATcov_RTS is

   pragma Pure;

   package Std renames Standard;
   package Sys renames System;

   Version : constant := 4;
   --  For compatibility with the GNATcoverage in use, GNATcov_RTS is
   --  versioned.
   --
   --  1 -- initial runtime version
   --  2 -- extend trace entry model to account for C files
   --  3 -- add a renaming of the Standard and System packages in GNATcov_RTS
   --  4 -- add C witness functions / buffer types

end GNATcov_RTS;
