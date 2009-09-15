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

--  This unit exposes very basic Geographical Map abstractions for the
--  field in the Explore example.

--  The field is very simply abstracted as a two dimensions array of
--  squares, each with an (x, y) position relative to (1, 1) for the
--  north-west corner.

with Links;

package Geomaps is

   ------------------------------
   -- Directions and Positions --
   ------------------------------

   type Direction is (West, North, East, South);

   subtype XYrange is Integer range 1 .. 9;
   type Position is record
      X, Y : XYrange;
   end record;

   ----------------
   -- Situations --
   ----------------

   --  A Situation on the field encompasses an actor position, the
   --  direction it is heading at and the kind of Square one step ahead
   --  in this direction.

   type Square is (Ground, Block, Water, Unknown);

   type Situation is record
      Pos : Position;
      Dir : Direction;
      Sqa : Square;
   end record;

   function Pos_Ahead_Of (Situ : Situation) return Position;
   --  Position of the square ahead the position in SITU, looking into
   --  the direction held there as well.

   package Situation_Links is new Links (Data_Type => Situation);

   -----------------------------------------
   -- The field map representation per se --
   -----------------------------------------

   type Geomap is array (XYrange'Range, XYrange'Range) of Square;
   Sqx : constant := 1;
   Sqy : constant := 2;

   procedure Dump (Map : Geomap; Situ : Situation);
   --  Printout field MAP to standard output, and represent a robot
   --  situation SITU on top.

end Geomaps;
