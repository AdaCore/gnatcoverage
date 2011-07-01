------------------------------------------------------------------------------
--                                                                          --
--                              Couverture                                  --
--                                                                          --
--                      Copyright (C) 2008-2011, AdaCore                    --
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

with Assert, Engines; use Engines;

------------------
-- Test_Engines --
------------------

procedure Test_Engines is
   E : Engine := (Speed => 0, Tolerance => 10, Mode => Safe);

begin
   Process (Cmd => (Kind => Accelerate, Increment => 5), E   => E);
   Assert (Stable (E));

   Process (Cmd => (Kind => Accelerate, Increment => 15), E   => E);
   Assert (Stable (E));

   Check (E);

   Process (Cmd => (Kind => Stop), E   => E);
   Assert (Stable (E));
end Test_Engines;
