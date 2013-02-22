------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Interfaces;         use Interfaces;
with GNAT.Byte_Swapping; use GNAT.Byte_Swapping;

package Swaps is

   function Swap is new Swapped2 (Integer_16);
   function Swap is new Swapped2 (Unsigned_16);
   function Swap is new Swapped4 (Integer_32);
   function Swap is new Swapped4 (Unsigned_32);
   function Swap is new Swapped8 (Integer_64);
   function Swap is new Swapped8 (Unsigned_64);

   procedure Swap_16 (V : in out Integer_16);
   procedure Swap_16 (V : in out Unsigned_16);
   procedure Swap_32 (V : in out Integer_32);
   procedure Swap_32 (V : in out Unsigned_32);
   procedure Swap_64 (V : in out Unsigned_64);
   procedure Swap_64 (V : in out Integer_64);

end Swaps;
