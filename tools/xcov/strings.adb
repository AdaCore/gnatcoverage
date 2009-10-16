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

with Ada.Strings.Hash;

package body Strings is

   ----------
   -- Hash --
   ----------

   function Hash (El : String_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (El.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (L, R : String_Access) return Boolean is
   begin
      pragma Assert (L /= null and then R /= null);
      return L.all = R.all;
   end Equal;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : String_Access) return Boolean is
   begin
      pragma Assert (L /= null and then R /= null);
      return L.all < R.all;
   end "<";

end Strings;
