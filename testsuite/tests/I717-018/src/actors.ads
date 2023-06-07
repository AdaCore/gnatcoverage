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

--  This unit exposes the root Actor abstraction for the Explore example,
--  meant to be used for active entities in the system, such as the robot
--  or the control station.
--
--  This is useful to allow associations of arbitrary entities to other
--  software components, for instance to support port/actor ownership.

package Actors is
   type Actor is abstract tagged private;
   type Actor_Ref is access all Actor'Class;

   procedure Kill (A : in out Actor'Class);
   --  Called when something bad happened to/in A and it is
   --  not to be relied upon any more.

   function Live (A : Actor'Class) return Boolean;
   --  True until A is killed

private
   type Actor is abstract tagged record
      Live : Boolean := True;
   end record;
end Actors;
