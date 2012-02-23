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

package body Ppc_Descs is

   function Get_Mask (Field : Ppc_Fields) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      pragma Assert (not (F.First = 0 and F.Last = 31));
      Nbr_Bits : constant Bit_Number := F.Last - F.First + 1;
   begin
      pragma Assert (F.First <= F.Last);
      return ((2 ** Nbr_Bits) - 1) * (2 ** (31 - F.Last));
   end Get_Mask;

   function Get_Field (Field : Ppc_Fields; V : Unsigned_32) return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.Last - F.First + 1;
   begin
      --  First shift to the left to remove left bits.
      --  Then shift to the right to put the value on the right.
      return Shift_Right (Shift_Left (V, F.First), 32 - Len);
   end Get_Field;

   function Get_Signed_Field (Field : Ppc_Fields; V : Unsigned_32)
                             return Unsigned_32
   is
      F : constant Field_Type := Fields_Mask (Field);
      Len : constant Natural := F.Last - F.First + 1;
   begin
      return Shift_Right_Arithmetic (Shift_Left (V, F.First), 32 - Len);
   end Get_Signed_Field;
end Ppc_Descs;
