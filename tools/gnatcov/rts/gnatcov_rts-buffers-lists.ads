------------------------------------------------------------------------------
--                                                                          --
--                   GNATcoverage Instrumentation Runtime                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

package GNATcov_RTS.Buffers.Lists is

   pragma Preelaborate;

   type GNATcov_RTS_Unit_Coverage_Buffers_Array is record
      Length  : aliased unsigned;
      Buffers : System.Address;
   end record;
   pragma Convention (C, GNATcov_RTS_Unit_Coverage_Buffers_Array);

   type Unit_Coverage_Buffers_Access is
     access all GNATcov_RTS_Unit_Coverage_Buffers;

   type Unit_Coverage_Buffers_Array is
     array (Positive range <>) of Unit_Coverage_Buffers_Access;

end GNATcov_RTS.Buffers.Lists;
