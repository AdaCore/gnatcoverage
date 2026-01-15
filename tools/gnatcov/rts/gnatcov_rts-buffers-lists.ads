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

package GNATcov_RTS.Buffers.Lists is

   pragma Preelaborate;

   type Coverage_Buffers_Access is
     access constant GNATcov_RTS_Coverage_Buffers;
   pragma Convention (C, Coverage_Buffers_Access);

   type Coverage_Buffers_Group is
     array (Positive range <>) of Coverage_Buffers_Access;
   pragma Convention (C, Coverage_Buffers_Group);

   type GNATcov_RTS_Coverage_Buffers_Group is record
      Length  : aliased unsigned;
      Buffers : System.Address;
      --  Address of a Coverage_Buffers_Group array of Length items
   end record;
   pragma Convention (C, GNATcov_RTS_Coverage_Buffers_Group);

   type Coverage_Buffers_Group_Access is
     access constant GNATcov_RTS_Coverage_Buffers_Group;
   pragma Convention (C, Coverage_Buffers_Group_Access);

   type Coverage_Buffers_Group_Array is
     array (Positive range <>) of Coverage_Buffers_Group_Access;
   pragma Convention (C, Coverage_Buffers_Group_Array);

   type GNATcov_RTS_Coverage_Buffers_Group_Array is record
      Length : aliased unsigned;
      Groups : System.Address;
      --  Address of a Coverage_Buffers_Group_Array array of Length items
   end record;
   pragma Convention (C, GNATcov_RTS_Coverage_Buffers_Group_Array);

   procedure Reset_Group_Array_Buffers
     (Arr : GNATcov_RTS_Coverage_Buffers_Group_Array);
   --  Set the components of all the buffers in each group to zero, effectively
   --  resetting the coverage state of all obligations to "not covered".

   Witness_Limited_Actual : Witness_Limited_Type;
   pragma
     Export
       (Convention => Ada,
        Entity => Witness_Limited_Actual,
        External_Name => "gnatcov_rts_witness_limited");
   --  See comment declaration of Witness_Limited in gnatcov_rts-buffers.ads

end GNATcov_RTS.Buffers.Lists;
