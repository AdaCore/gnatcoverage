--  This procedure contains record object declaration with explicit
--  initialization expression

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   Coordinate_V : Coordinate := Coordinate_Zero;       -- # dcls
begin
   Res   := not Res;                                   -- # stmt
end Decls;

