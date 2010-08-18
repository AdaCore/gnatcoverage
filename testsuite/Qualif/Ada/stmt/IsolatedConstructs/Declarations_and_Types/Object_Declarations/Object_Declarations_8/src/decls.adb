--  This procedure contains an object of a derived record type declaration with
--  no explicit initialization, but the parent record type does have default
--  initialization expressions for components.

with Decls_Support; use Decls_Support;
with Support; use Support;
procedure Decls (Res : in out Boolean) is
   Derived_Coordinate_V : Derived_Coordinate; -- # dcls
begin
   Res := not Res;                            -- # stmt
end Decls;

