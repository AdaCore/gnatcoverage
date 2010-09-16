--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Var1 : T_Private := Get_Private (10);
   Var2 : T_Private := Get_Private (20);
begin
   Swap (Var1, Var2);
   Assert (Get_Integer (Var1)  = 20 and then Get_Integer (Var2)  = 10);

   Assert (Get_Integer (T_Private_V) = 0);

   Assert (Get_Integer (Local_Fun (Var2)) = 0);
end Test_Obj_Decls_Part;

--# swap.adb
-- /stmt/       l+ 0

--# decls_pack.adb
-- /local_swap/ l- s-

-- /decl/       l+ 0
-- /stmt/       l+ 0
-- /case1/      l- s-
-- /case2/      l- s-
-- /case3/      l+ 0

--# decls_pack.ads
-- /dcls/       l+ 0
