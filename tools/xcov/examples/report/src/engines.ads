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

--  This package exposes a very basic and incomplete Engine abstraction

package Engines is

   --  The engine runs at some Speed and is stable while this remains below
   --  a given Tolerance threshold. It will process Commands in a way that
   --  depends on a current operational mode:

   type Operating_Mode is
     (Dumb, --  In Dumb mode, the engine executes any command, even
            --  if this makes it unstable

      Safe  --  In Safe mode, the engine discards commands that would
            --  make it unstable
     );

   type Engine is record
      Speed, Tolerance : Natural;
      Mode : Operating_Mode;
   end record;

   --  We support two kinds of commands only:

   type Command_Kind is
     (Accelerate, --  Augment engine speed by a provided amount
      Stop        --  Reset engine speed to 0
     );

   type Command (Kind : Command_Kind) is record
      case Kind is
         when Accelerate => Increment : Integer;
         when others     => null;
      end case;
   end record;

   --------------------
   -- User interface --
   --------------------

   procedure Process (Cmd : Command; E : in out Engine);
   --  Query engine E to process command CMD

   function Stable (E : Engine) return Boolean;
   --  Whether engine E's current speed is below its stability tolerance
   --  threshold

   procedure Check (E : in out Engine);
   --  Stop E if unstable and in Safe mode and

end Engines;
