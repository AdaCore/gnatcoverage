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

-----------------
-- Test_Stacks --
-----------------

--  Exercise the Stacks abstraction

with Stacks, Support;

procedure Test_Stacks is
   package Integer_Stacks is new Stacks (Data_Type => Integer);
   use Integer_Stacks;

   X : Integer;
   S : Integer_Stacks.Stack (Capacity => 1);
begin
   Push (12, S);
   Pop (X, S);
   if X /= 12 then
      raise Program_Error;
   end if;
end Test_Stacks;
