--  This procedure contains record type declaration with component definition
--  that contains an explicit constraint.

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   type Rec is record Comp : Vector (L .. R); end record; -- # dcls
begin
   declare
      Tmp : Rec := (Comp => (others => 0)); -- # stmt
   begin
      Res := X in  Tmp.Comp'Range;           -- # stmt
   end;
end Decls;

