--  This procedure contains scalar variable object declaration with explicit
--  initialization expression

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Integer_Swap (I, J : in out Integer) is
   Tmp : Integer := I; -- # stmt
begin
   I := J;             -- # stmt
   J := Tmp;           -- # stmt
end Integer_Swap;

