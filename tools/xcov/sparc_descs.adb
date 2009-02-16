------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                        Copyright (C) 2009, AdaCore                       --
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

with System.Storage_Elements;
with Ada.Unchecked_Conversion;

package body Sparc_Descs is
   function Read_Byte (Addr : Address) return Unsigned_8;

   function Read_Byte (Addr : Address) return Unsigned_8
   is
      type Unsigned_8_Acc is access all Unsigned_8;
      function To_Unsigned_8_Acc is new Ada.Unchecked_Conversion
        (Address, Unsigned_8_Acc);
   begin
      return To_Unsigned_8_Acc (Addr).all;
   end Read_Byte;

   function Get_Insn (Addr : Address) return Unsigned_32
   is
      use System.Storage_Elements;
      B0, B1, B2, B3 : Unsigned_8;
   begin
      B0 := Read_Byte (Addr + 0);
      B1 := Read_Byte (Addr + 1);
      B2 := Read_Byte (Addr + 2);
      B3 := Read_Byte (Addr + 3);
      return Shift_Left (Unsigned_32 (B0), 24)
        or Shift_Left (Unsigned_32 (B1), 16)
        or Shift_Left (Unsigned_32 (B2), 8)
        or Shift_Left (Unsigned_32 (B3), 0);
   end Get_Insn;
end Sparc_Descs;
