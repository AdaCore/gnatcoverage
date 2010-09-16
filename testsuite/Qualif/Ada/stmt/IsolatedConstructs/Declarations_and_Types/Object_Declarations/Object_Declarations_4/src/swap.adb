--  This procedure contains declaration of an object of a private type with
--  explicit initialization expression
with Decls_Support; use Decls_Support;
with Support;       use Support;
procedure Swap (V1, V2 : in out T_Private) is
   Tmp : T_Private := V1;  -- # stmt
begin
   V1 := V2;               -- # stmt
   V2 := Tmp;              -- # stmt
end Swap;

