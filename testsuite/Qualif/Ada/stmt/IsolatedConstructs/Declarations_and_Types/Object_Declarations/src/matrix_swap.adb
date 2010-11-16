--  This procedure contains array variable object declaration with explicit
--  initialization expression

with Decls_Support; use Decls_Support;
with Support;       use Support;
procedure Matrix_Swap (M1, M2 : in out Matrix) is
   Tmp : Matrix := M1; -- # stmt
begin
   M1 := M2;           -- # stmt
   M2 := Tmp;          -- # stmt
end Matrix_Swap;

