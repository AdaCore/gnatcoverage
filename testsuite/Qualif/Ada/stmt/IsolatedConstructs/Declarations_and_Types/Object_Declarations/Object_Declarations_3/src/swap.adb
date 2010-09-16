--  This procedure contains record object declaration with explicit
--  initialization expression
with Decls_Support; use Decls_Support;
with Support;       use Support;
procedure Swap (C1, C2 : in out Coordinate) is
   Tmp : Coordinate := C1;  -- # stmt
begin
   C1 := C2;                -- # stmt
   C2 := Tmp;               -- # stmt
end Swap;

