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

package body Stacks is

   -------------------------
   -- Classical accessors --
   -------------------------

   function Size (S : Stack) return Natural;

   function Size (S : Stack) return Natural is
   begin
      return S.Size;
   end Size;

   function Full (S : Stack) return Boolean is
   begin
      return Size (S) = S.Capacity;
   end Full;

   function Empty (S : Stack) return Boolean is
   begin
      return Size (S) = 0;
   end Empty;

   procedure Pop (Item : out Data_Type; S : in out Stack) is
   begin
      if Empty (S) then
         raise Program_Error;
      end if;
      Item := S.Items (S.Size);
      S.Size := S.Size - 1;
   end Pop;

   procedure Push (Item : Data_Type; S : in out Stack) is
   begin
      if Full (S) then
         raise Program_Error;
      end if;
      S.Size := S.Size + 1;
      S.Items (S.Size) := Item;
   end Push;

end Stacks;
