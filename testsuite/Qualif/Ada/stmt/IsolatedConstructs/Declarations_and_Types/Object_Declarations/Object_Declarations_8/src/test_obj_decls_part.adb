--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Var1 : Derived_Discrete_Coordinate := (10, 100);
   Var2 : Derived_Discrete_Coordinate := (20, 200);

   V : Derived_Coordinate;
begin
   Swap (Var1, Var2);
   Assert (Var1.X = 20  and then
           Var1.Y = 200 and then
           Var2.X = 10  and then
           Var2.Y = 100);

   Assert (Derived_Discrete_Coordinate_V.X = 0 and then
           Derived_Discrete_Coordinate_V.Y = 0);

   V := Local_Fun (1.0, 2.0);
   Assert (V.X = 1.0 and then V.Y = 2.0);
end Test_Obj_Decls_Part;

--# swap.adb
-- /stmt/          l+ 0
-- /if/            l+ 0

--# decls_pack.adb
-- /local_swap/    l- s-
-- /if_local_swap/ l- s-

-- /decl/          l+ 0
-- /stmt/          l+ 0
-- /in_if/         l+ 0

--# decls_pack.ads
-- /dcls/          l+ 0
