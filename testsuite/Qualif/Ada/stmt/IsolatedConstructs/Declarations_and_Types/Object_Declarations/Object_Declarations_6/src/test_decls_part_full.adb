--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Coord_Var1 : Access_Coordinate := new Coordinate'(1.0, 2.0);
   Coord_Var2 : Access_Coordinate := new Coordinate'(3.0, 4.0);

   Var1 : Access_Integer := new Integer'(1);
   Var2 : Access_Integer := new Integer'(2);
begin
   Swap (Coord_Var1, Coord_Var2);
   Assert (Coord_Var1.X = 3.0 and then
           Coord_Var1.Y = 4.0 and then
           Coord_Var2.X = 1.0 and then
           Coord_Var2.Y = 2.0);

   Local_Swap (Var1, Var2);
   Assert (Var1.all = 2 and then Var2.all = 1);

   Assert (Local_Fun (1).all = 1);
   Assert (Local_Fun (-1) = null);

end Test_Decls_Part_Full;

--# swap.adb
-- /stmt/          l+ 0

--# decls_pack.adb
-- /local_swap/    l+ 0

-- /decl/          l+ 0
-- /stmt/          l+ 0
-- /in_if/         l+ 0

--# decls_pack.ads
-- /dcls/          l+ 0
