--  This procedure contains integer object declaration with subtype indication
--  having an explicit range constraint. There is no explicit initialization in
--  this object declaration

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls
  (Res  : out Boolean;
   X, L, R : in Integer)
is
   Tmp : Integer range L .. R;  -- # dcls
begin
   Tmp := L;                    -- # stmt
   Res := X in  Tmp .. R;       -- # stmt
end Decls;

