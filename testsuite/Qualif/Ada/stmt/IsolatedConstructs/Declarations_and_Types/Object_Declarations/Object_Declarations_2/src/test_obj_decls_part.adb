--  Test driver for object declarations. The goal is to have not all but a part
--  of the declarations of interest covered, so it calls not all but some of
--  the subprograms from the functional code

with Swap;
with Decls_Pack;    use Decls_Pack;
with Decls_Support; use Decls_Support;
with Support;       use Support;

procedure Test_Obj_Decls_Part is
   Matr1 : Matrix := (1 => (1 => 1));
   Matr2 : Matrix := (1 => (1 => 2));
begin
   Swap (Matr1, Matr2);
   Assert (Matr1 (1, 1) = 2 and then Matr2 (1, 1) = 1);

   Assert (Matrix_C (2, 2) = 4);

   Assert (Local_Fun (Matr1) (1, 1) = -2);
end Test_Obj_Decls_Part;

--# swap.adb
--  /stmt/       l+ 0

--# decls_pack.adb
--  /local_swap/ l- s-

--  /decl/       l+ 0
--  /stmt/       l+ 0

--# decls_pack.ads
--  /dcls/       l+ 0
