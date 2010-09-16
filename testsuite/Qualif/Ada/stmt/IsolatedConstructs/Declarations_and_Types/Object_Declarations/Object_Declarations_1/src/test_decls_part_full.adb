--  Test driver for object declarations. It calls everything from the
--  functional code, so everything is expected to be reported as covered.

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Decls_Part_Full is
   Int1 : Integer := 1;
   Int2 : Integer := 2;
begin
   Swap (Int1, Int2);
   Assert (Int1 = 2 and then Int2 = 1);

   Assert (Decls_Pack.Count = 0);

   Assert (Local_Fun (Mon) = Tue);
   Assert (Local_Fun (Sun) = Mon);
end Test_Decls_Part_Full;

--# swap.adb
--  /stmt/       l+ 0

--# decls_pack.adb
--  /local_swap/ l- s-

--  /decl/       l+ 0
--  /stmt/       l+ 0
--  /case1/      l+ 0
--  /case2/      l+ 0

--# decls_pack.ads
--  /dcls/       l+ 0
