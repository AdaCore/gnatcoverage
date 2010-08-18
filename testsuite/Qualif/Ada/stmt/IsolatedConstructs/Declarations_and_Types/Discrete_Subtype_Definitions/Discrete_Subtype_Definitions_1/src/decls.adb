--  This procedure contains array type declaration with discrete subtype
--  definition that contains an explicit constraint.

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   type Arr is array (Integer range L .. R) of Integer; -- # dcls
begin
   declare
      Tmp : Arr := (others => 1);                        -- # stmt
   begin
      Res := X in  Tmp'Range;                            -- # stmt
   end;
end Decls;

