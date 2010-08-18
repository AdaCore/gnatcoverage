--  This procedure contains access object declaration with explicit
--  initialization

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   Access_Coordinate_V : Access_Coordinate := new Coordinate; -- # dcls
begin
   Res   := not Res;                                          -- # stmt
end Decls;

