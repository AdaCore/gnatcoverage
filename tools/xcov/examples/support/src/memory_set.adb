------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Unchecked_Conversion;

package body Memory_Set is

   ------------
   -- memset --
   ------------

   function Memset (M : Address; C : int; Size : size_t) return Address is
      subtype Mem_Array is char_array (size_t);
      type Mem_Ptr is access Mem_Array;

      function To_Memptr is
         new Ada.Unchecked_Conversion (Address, Mem_Ptr);

      Dest : constant Mem_Ptr := To_Memptr (M);

   begin
      if Size > 0 then
         for J in 0 .. Size - 1 loop
            Dest (J) := char'Val (C);
         end loop;
      end if;

      return M;
   end Memset;

end Memory_Set;
