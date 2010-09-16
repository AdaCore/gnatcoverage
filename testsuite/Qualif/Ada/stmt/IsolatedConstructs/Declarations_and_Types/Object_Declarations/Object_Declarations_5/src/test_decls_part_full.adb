--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Var1 : Discrete_Coordinate := (10, 100);
   Var2 : Discrete_Coordinate := (20, 200);

   V1 : Coordinate := (1.0, 10.0);
   V2 : Coordinate := (2.0, 20.0);
begin
   Swap (Var1, Var2);
   Assert (Var1.X = 20  and then
           Var1.Y = 200 and then
           Var2.X = 10  and then
           Var2.Y = 100);

   Local_Swap (V1, V2);
   Assert (V1.X = 2.0  and then
           V1.Y = 20.0 and then
           V2.X = 1.0  and then
           V2.Y = 10.0);

   Assert (Discrete_Coordinate_V.X = 0 and then Discrete_Coordinate_V.Y = 0);

   Var1 := Local_Fun (1, 2);
   Assert (Var1.X = 1 and then Var1.Y = 2);
end Test_Decls_Part_Full;

--# swap.adb
-- /stmt/          l+ 0
-- /if/            l+ 0

--# decls_pack.adb
-- /local_swap/    l+ 0
-- /if_local_swap/ l+ 0

-- /decl/          l+ 0
-- /stmt/          l+ 0
-- /in_if/         l+ 0

--# decls_pack.ads
-- /dcls/          l+ 0
