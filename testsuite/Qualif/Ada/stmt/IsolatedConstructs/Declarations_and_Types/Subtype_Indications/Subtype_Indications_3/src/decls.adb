--  This procedure contains object declaration with subtype indication having
--  an explicit discriminant constraint, and there is a component of the object
--  type that depends on discriminant. There is no explicit initialization in
--  this object declaration

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   Tmp : Var_String (X);                   -- # dcls
begin
   Tmp.Data := (others => ' ');            -- # stmt
   Res      := Tmp.Data'Length in  L .. R; -- # stmt
end Decls;

