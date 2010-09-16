--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Coord1 : Coordinate := Coordinate_Zero;
   Coord2 : Coordinate := (1.0, 1.0);
begin
   Swap (Coord1, Coord2);
   Assert (Coord2 = Coordinate_Zero and then Coord1 = (1.0, 1.0));

   Local_Swap (Coord1, Coord2);
   Assert (Coord1 = Coordinate_Zero and then Coord2 = (1.0, 1.0));

   Assert (My_String.Data = "Ada");

   Assert (Local_Fun (My_String).Data = "Beb");
end Test_Decls_Part_Full;

--# swap.adb
--  /stmt/       l+ 0

--# decls_pack.adb
--  /local_swap/ l+ 0

--  /decl/       l+ 0
--  /stmt/       l+ 0

--# decls_pack.ads
--  /dcls/       l+ 0
