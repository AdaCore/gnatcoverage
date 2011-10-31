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

--  This package offers a simple "bounded queue" data structure abstraction

generic
   type Data_Type is private;  --  The elements data type
package Queues is
   type Queue (Capacity : Natural) is private;

   function Full (Q : Queue) return Boolean;
   --  Whether Q is full with respect to its Capacity.

   function Empty (Q : Queue) return Boolean;
   --  Whether Q is empty.

   procedure Push (Item : Data_Type; Q : in out Queue);
   --  Push Item at the back of queue Q if it is not Full,
   --  raise Program_Error and leave Q unchanged otherwise.

   procedure Pop (Item : out Data_Type; Q : in out Queue);
   --  Pop the top element off the head of Q into Item if Q is not Empty,
   --  raise Program_Error and leave Item undefined otherwise.

private

   type Data_Array is array (Natural range <>) of Data_Type;
   type Queue (Capacity : Natural) is record
      Items : Data_Array (1 .. Capacity);
      Size : Natural := 0;
      Front, Back : Natural := 1;
   end record;
end Queues;
