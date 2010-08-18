--  This procedure contains array type declaration with discrete subtype
--  definition that contains an explicit constraint.

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   Arr : array (Integer range L .. R) of Integer; -- # dcls
begin
   Arr := (others => 1);                          -- # stmt
   Res := X in  Arr'Range;                        -- # stmt
end Decls;

