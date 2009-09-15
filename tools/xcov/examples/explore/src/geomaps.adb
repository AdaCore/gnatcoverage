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

with Text_IO; use Text_IO;

package body Geomaps is

   --  Characters to printout for each possible kind of field Square,
   --  and to represent an actor heading to a specific direction.

   Square_Image : array (Square) of Character
     := (Ground => ' ', Block => '#', Water => '~', Unknown => '.');

   Situ_Image : array (Direction) of Character
     := (North => '^', West => '<', East => '>', South => 'v');

   ----------
   -- Dump --
   ----------

   procedure Dump (Map : Geomap; Situ : Situation) is
   begin
      New_Line;
      for Y in Map'Range (Sqy) loop
         for X in Map'Range (Sqx) loop
            if Situ.Pos = (X, Y) then
               Put (Situ_Image (Situ.Dir));
            else
               Put (Square_Image (Map (X, Y)));
            end if;
         end loop;
         New_Line;
      end loop;
   end Dump;

   ------------------
   -- Pos_Ahead_Of --
   ------------------

   function Pos_Ahead_Of (Situ : Situation) return Position is
      subtype Move_XYrange is Integer range -1 .. +1;

      type Move is record
         Dx, Dy : Move_XYrange;
      end record;

      XYmoves : constant array (Direction) of Move
        := (West => (Dx => -1, Dy => 0), East => (Dx => +1, Dy => 0),
            South => (Dx => 0, Dy => +1), North => (Dx => 0, Dy => -1));

      XYmove : Move renames XYmoves (Situ.Dir);
   begin
      return (X => Situ.Pos.X + XYmove.Dx, Y => Situ.Pos.Y + XYmove.Dy);
   end Pos_Ahead_Of;

end Geomaps;
