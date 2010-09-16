--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Var1 : T_Private := Get_Private (10);
   Var2 : T_Private := Get_Private (20);
begin
   Swap (Var1, Var2);
   Assert (Get_Integer (Var1)  = 20 and then Get_Integer (Var2)  = 10);

   Local_Swap (Var1, Var2);
   Assert (Get_Integer (Var2)  = 20 and then Get_Integer (Var1)  = 10);

   Assert (Get_Integer (T_Private_V) = 0);

   Assert (Get_Integer (Local_Fun (Var2)) = 0);
   Assert (Get_Integer (Local_Fun (Get_Private (1))) = 100);
   Assert (Get_Integer (Local_Fun (Get_Private (2))) = 0);
end Test_Decls_Part_Full;

--# swap.adb
-- /stmt/       l+ 0

--# decls_pack.adb
-- /local_swap/ l+ 0

-- /decl/       l+ 0
-- /stmt/       l+ 0
-- /case1/      l+ 0
-- /case2/      l+ 0
-- /case3/      l+ 0

--# decls_pack.ads
-- /dcls/       l+ 0
