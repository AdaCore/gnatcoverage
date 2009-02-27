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

with Queues;

procedure Test_Queues is

   package Integer_Queues is new Queues (Data_Type => Integer);
   use Integer_Queues;

   X : Integer;
   Q : Integer_Queues.Queue (Capacity => 3);
begin
   if not Empty (Q) then
      raise Program_Error;
   end if;

   Push (1, Q);
   Push (2, Q);
   Push (3, Q);

   Pop (X, Q);
   if X /= 1 then
      raise Program_Error;
   end if;

   Push (4, Q);
   if not Full (Q) then
      raise Program_Error;
   end if;

   Pop (X, Q);
   if X /= 2 then
      raise Program_Error;
   end if;
   Pop (X, Q);
   if X /= 3 then
      raise Program_Error;
   end if;
   Pop (X, Q);
   if X /= 4 then
      raise Program_Error;
   end if;

   if not Empty (Q) then
      raise Program_Error;
   end if;
end Test_Queues;
