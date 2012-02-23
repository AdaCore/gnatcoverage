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

with Swaps;  use Swaps;
with Traces; use Traces;

package body Disa_Common is

   -----------------------
   -- To_Big_Endian_U32 --
   -----------------------

   function To_Big_Endian_U32 (Bin : Binary_Content) return Unsigned_32 is
      pragma Assert (Bin'Length = 4);

      Bin_Aligned : Binary_Content (1 .. 4) := Bin;
      for Bin_Aligned'Alignment use Unsigned_32'Alignment;

      Result : Unsigned_32;
      pragma Import (Ada, Result);
      for Result'Address use Bin_Aligned'Address;
   begin
      if not Big_Endian_Host then
         Swap_32 (Result);
      end if;
      return Result;
   end To_Big_Endian_U32;

end Disa_Common;
