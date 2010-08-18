--  This procedure contains a declaration of an object of a private type with
--  explicit initialization expression

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   T_Private_V : T_Private := T_Private_Zero;   -- # dcls
begin
   Res   := not Res;                            -- # stmt
end Decls;

