--  This procedure contains constant array object declaration with explicit
--  initialization expression

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   M : constant Matrix := ((Identity (1), 2), (3, 4)); -- # dcls
begin
   Res   := not Res;                                   -- # stmt
end Decls;

