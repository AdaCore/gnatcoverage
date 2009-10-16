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
with GNAT.Strings; use GNAT.Strings;

package Strings is

   subtype File_Name is String_Access;

   function Hash (El : String_Access) return Ada.Containers.Hash_Type;
   --  Compute a hash from El.all

   function Equal (L, R : String_Access) return Boolean;
   --  Assuming that L and R are not null, return true iff L and R designate
   --  identical strings.
   --  We do not redefine "=" here, so that a String_Access can be compared to
   --  null.

   function "<" (L, R : String_Access) return Boolean;
   --  Assuming that L and R are not null, return true iff L.all < R.all

private
   pragma Inline (Hash);
   pragma Inline (Equal);
   pragma Inline ("<");
end Strings;
