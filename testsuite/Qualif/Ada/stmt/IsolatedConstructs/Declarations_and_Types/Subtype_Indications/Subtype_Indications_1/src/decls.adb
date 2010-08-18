--  This procedure contains declaration of an integer subtype with explicit
--  range constraint

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   subtype My_Range is Integer range L .. R;      -- # dcls
begin
   Res := X in  My_Range;     -- # stmt
end Decls;

