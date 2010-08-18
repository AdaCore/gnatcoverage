--  This procedure contains record object declaration with implicit
--  initialization

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   Coordinate_V : Coordinate;       -- # dcls
begin
   Res   := not Res;                -- # stmt
end Decls;

