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
with Ada.Containers;
with Ada.Unchecked_Deallocation;

package Strings is
   type String_Acc is access String;

   subtype File_Name is String_Acc;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (String, String_Acc);

   function Hash (El : String_Acc) return Ada.Containers.Hash_Type;
   --  Compute an hash from EL.

   function Equal (L, R : String_Acc) return Boolean;
   --  Return true iff L and R designate the same string.

   function Less_Than (L, R : String_Acc) return Boolean;
   --  Return true iff L.all < R.all.

private
   pragma Inline (Hash);
   pragma Inline (Equal);
   pragma Inline (Less_Than);
end Strings;
