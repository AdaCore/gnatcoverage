------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                    Copyright (C) 2008-2009, AdaCore                      --
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
