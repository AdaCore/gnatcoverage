--  This procedure contains array type declaration with component definition
--  that contains an explicit constraint.

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   type Arr is array (Week_Day) of Integer range L .. R; -- # dcls
begin
   declare
      Tmp : Arr := (Mon .. Fri => L, Sat .. Sun => R);   -- # stmt
   begin
      Res := X in  Tmp (Tmp'First) .. Tmp (Tmp'Last);    -- # stmt
   end;
end Decls;

