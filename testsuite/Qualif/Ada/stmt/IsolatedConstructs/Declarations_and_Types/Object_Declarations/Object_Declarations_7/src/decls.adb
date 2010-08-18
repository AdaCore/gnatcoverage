--  This procedure contains access object declaration with no explicit
--  initialization

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   Access_Coordinate_V : Access_Coordinate; -- # dcls
begin
   Access_Coordinate_V := new Coordinate;   -- # stmt
   Res := not Res;                          -- # stmt
end Decls;

