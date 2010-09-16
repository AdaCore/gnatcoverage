--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Var1 : Access_Coordinate := new Coordinate'(1.0, 2.0);
   Var2 : Access_Coordinate := new Coordinate'(3.0, 4.0);
begin
   Swap (Var1, Var2);
   Assert (Var1.X = 3.0 and then
           Var1.Y = 4.0 and then
           Var2.X = 1.0 and then
           Var2.Y = 2.0);

   Assert (Access_All_Integer_Var = null);

   Assert (Local_Fun (1).all = 1);
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
