--  This procedure contains variable scalar object declaration with explicit
--  initialization expression

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   I : Integer := 1;     -- # dcls
begin
   Res   := not Res;     -- # stmt
end Decls;

