------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
--                                                                          --
-- Couverture is free software; you can redistribute it  and/or modify it   --
-- under terms of the GNU General Public License as published by the Free   --
-- Software Foundation; either version 2, or (at your option) any later     --
-- version.  Couverture is distributed in the hope that it will be useful,  --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details. You  should  have  received a copy of the GNU --
-- General Public License  distributed with GNAT; see file COPYING. If not, --
-- write  to  the Free  Software  Foundation,  59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
------------------------------------------------------------------------------
package body Swaps is
   function Swap_16 (V : Unsigned_16) return Unsigned_16 is
   begin
      return Shift_Left ((V and 16#Ff#), 8)
        or (Shift_Right (V, 8) and 16#Ff#);
   end Swap_16;

   function Swap_32 (V : Unsigned_32) return Unsigned_32 is
   begin
      return Shift_Left ((V and 16#Ff#), 24)
        or Shift_Left ((V and 16#Ff00#), 8)
        or Shift_Right (V and 16#Ff0000#, 8)
        or Shift_Right (V and 16#Ff000000#, 24);
   end Swap_32;

   function Swap_64 (V : Unsigned_64) return Unsigned_64 is
   begin
      return Shift_Left ((V and 16#Ff#), 56)
        or Shift_Left ((V and 16#Ff00#), 40)
        or Shift_Left ((V and 16#Ff0000#), 24)
        or Shift_Left ((V and 16#Ff000000#), 8)
        or Shift_Right (V and 16#Ff_00000000#, 8)
        or Shift_Right (V and 16#Ff00_00000000#, 24)
        or Shift_Right (V and 16#Ff0000_00000000#, 40)
        or Shift_Right (V and 16#Ff000000_00000000#, 56);
   end Swap_64;

   procedure Swap_16 (V : in out Unsigned_16) is
   begin
      V := Swap_16 (V);
   end Swap_16;

   procedure Swap_32 (V : in out Unsigned_32) is
   begin
      V := Swap_32 (V);
   end Swap_32;

   procedure Swap_64 (V : in out Unsigned_64) is
   begin
      V := Swap_64 (V);
   end Swap_64;
end Swaps;
