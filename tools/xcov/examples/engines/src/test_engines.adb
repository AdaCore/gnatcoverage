------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2009, AdaCore                    --
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

with Last_Chance_Handler;
with Engines; use Engines;

------------------
-- Test_Engines --
------------------
procedure Test_Engines is
   E : Engine;
begin

   --  Exercise various conditions and decisions in State_Of.  Incomplete, as
   --  this doesn't check the behavior on equality wrt thresholds, and misses
   --  the Alarming combinations.

   E.T := Stable_T + 1;
   E.P := Stable_P + 1;
   if State_Of (E) /= Critical then
      raise Program_Error;
   end if;

   E.T := Stable_T - 1;
   E.P := Stable_P - 1;
   if State_Of (E) /= Stable then
      raise Program_Error;
   end if;

end Test_Engines;
