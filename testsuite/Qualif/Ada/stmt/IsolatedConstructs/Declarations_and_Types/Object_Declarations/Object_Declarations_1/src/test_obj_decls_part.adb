--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Int1 : Integer := 1;
   Int2 : Integer := 2;
begin
   Swap (Int1, Int2);
   Assert (Int1 = 2 and then Int2 = 1);

   Assert (Decls_Pack.Count = 0);

   Assert (Local_Fun (Sat) = Sun);
end Test_Obj_Decls_Part;

--# swap.adb
--  /stmt/       l+ 0

--# decls_pack.adb
--  /local_swap/ l- s-

--  /decl/       l+ 0
--  /stmt/       l+ 0
--  /case1/      l+ 0
--  /case2/      l- s-

--# decls_pack.ads
--  /dcls/       l+ 0
