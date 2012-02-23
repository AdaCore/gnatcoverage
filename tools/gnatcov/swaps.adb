------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

package body Swaps is

   -------------
   -- Swap_16 --
   -------------

   procedure Swap_16 (V : in out Unsigned_16) is
   begin
      Swap2 (V'Address);
   end Swap_16;

   -------------
   -- Swap_32 --
   -------------

   procedure Swap_32 (V : in out Unsigned_32) is
   begin
      Swap4 (V'Address);
   end Swap_32;

   -------------
   -- Swap_64 --
   -------------

   procedure Swap_64 (V : in out Unsigned_64) is
   begin
      Swap8 (V'Address);
   end Swap_64;

end Swaps;
