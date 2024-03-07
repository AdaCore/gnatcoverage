------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

--  Basic types to use in source trace buffers. We try to avoid using types
--  from Interfaces and Interfaces.C, and in general to minimize the set of
--  dependencies of GNATcov_RTS on the Ada runtime, so that we can compute code
--  coverage for these runtime units.
--
--  This unit needs to be compilable with Ada 95 compilers

with System;

package GNATcov_RTS.Types is

   pragma Pure;
   pragma Warnings (Off);
   pragma No_Elaboration_Code_All;
   pragma Warnings (On);

   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_64 is mod 2 ** 64;

   --  We assume here that Integer (Ada) is a correct mapping for int (C)

   type int is new Integer;
   type unsigned is mod 2 ** int'Size;
   type size_t is mod System.Memory_Size;

end GNATcov_RTS.Types;
