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

with Interfaces;         use Interfaces;
with GNAT.Byte_Swapping; use GNAT.Byte_Swapping;

package Swaps is

   function Swap is new Swapped2 (Unsigned_16);
   function Swap is new Swapped4 (Unsigned_32);
   function Swap is new Swapped8 (Unsigned_64);

   procedure Swap_16 (V : in out Unsigned_16);
   procedure Swap_32 (V : in out Unsigned_32);
   procedure Swap_64 (V : in out Unsigned_64);

end Swaps;
