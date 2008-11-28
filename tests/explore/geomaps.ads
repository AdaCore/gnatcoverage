----------------------------------------------------------------------------
--                             GEOMAPS (SPEC)                             --
----------------------------------------------------------------------------

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

   subtype XYrange is Integer range 1 .. 21;
   type Position is record
      X, Y : XYrange;
   end record;

   ----------------
   -- Situations --
   ----------------

   --  A Situation on the field encompasses an actor position, the direction
   --  it is heading at (e.g. the robot front camera), and the kind of Square
   --  one step ahead in this direction.

   type Square is (Clear, Block, Unknown);

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

end;
