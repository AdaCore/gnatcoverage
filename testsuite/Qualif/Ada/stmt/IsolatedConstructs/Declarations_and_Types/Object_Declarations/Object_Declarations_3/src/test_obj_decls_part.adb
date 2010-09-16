--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Coord1 : Coordinate := Coordinate_Zero;
   Coord2 : Coordinate := (1.0, 1.0);
begin
   Swap (Coord1, Coord2);
   Assert (Coord2 = Coordinate_Zero and then Coord1 = (1.0, 1.0));

   Assert (My_String.Data = "Ada");

   Assert (Local_Fun (My_String).Data = "Beb");
end Test_Obj_Decls_Part;

--# swap.adb
--  /stmt/       l+ 0

--# decls_pack.adb
--  /local_swap/ l- s-

--  /decl/       l+ 0
--  /stmt/       l+ 0

--# decls_pack.ads
--  /dcls/       l+ 0
