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

package body Queues is

   -------------------------
   -- Classical accessors --
   -------------------------

   function Size (Q : Queue) return Natural;
   --  The number of items available in Q

   function Size (Q : Queue) return Natural is
   begin
      return Q.Size;
   end Size;

   function Full (Q : Queue) return Boolean is
   begin
      return Size (Q) = Q.Capacity;
   end Full;

   function Empty (Q : Queue) return Boolean is
   begin
      return Size (Q) = 0;
   end Empty;

   procedure Pop (Item : out Data_Type; Q : in out Queue) is
   begin
      if Empty (Q) then
         raise Program_Error;
      end if;

      Item := Q.Items (Q.Front);
      if Q.Front = Q.Items'Last then
         Q.Front := Q.Items'First;
      else
         Q.Front := Q.Front + 1;
      end if;
      Q.Size := Q.Size - 1;
   end Pop;

   procedure Push (Item : Data_Type; Q : in out Queue) is
   begin
      if Full (Q) then
         raise Program_Error;
      end if;

      Q.Items (Q.Back) := Item;
      if Q.Back = Q.Items'Last then
         Q.Back := Q.Items'First;
      else
         Q.Back := Q.Back + 1;
      end if;
      Q.Size := Q.Size + 1;
   end Push;

end Queues;
