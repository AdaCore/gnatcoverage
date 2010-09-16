--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Var1 : Derived_Discrete_Coordinate := (10, 100);
   Var2 : Derived_Discrete_Coordinate := (20, 200);

   V1 : Derived_Coordinate := (1.0, 10.0);
   V2 : Derived_Coordinate := (2.0, 20.0);

    V : Derived_Coordinate;
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

   Assert (Derived_Discrete_Coordinate_V.X = 0 and then
           Derived_Discrete_Coordinate_V.Y = 0);

   V := Local_Fun (1.0, 2.0);
   Assert (V.X = 1.0 and then V.Y = 2.0);
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
