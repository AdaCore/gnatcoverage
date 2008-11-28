----------------------------------------------------------------------------
--                             GEOMAPS (BODY)                             --
----------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body Geomaps is

   --  Characters to printout for each possible kind of field Square,
   --  and to represent an actor heading into specific direction.

   Square_Image : array (Square) of Character
     := (Clear => ' ', Block => '#', Unknown => '?');

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
   end;

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
   end;

end;
