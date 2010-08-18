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

--  This package exposes a very basic and incomplete Engine abstraction

package Engines is

   --  The Engine abstraction per se

   type Engine is record
      P, T : Integer; --  Internal state: Pressure & Temperature
   end record;

   --  Pressure and Temperature thresholds wrt engine stability

   Stable_P : constant := 10;
   Stable_T : constant := 50;

   function Stable (E : Engine) return Boolean;
   --  Whether the engine E is stable, iff both Pressure and Temperature
   --  are below the stability threshold.

end Engines;
