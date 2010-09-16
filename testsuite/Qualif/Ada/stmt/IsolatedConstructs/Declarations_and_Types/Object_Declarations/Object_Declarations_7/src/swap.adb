--  This procedure contains access object declaration with no explicit
--  initialization
with Decls_Support; use Decls_Support;
with Support;       use Support;
procedure Swap (V1, V2 : in out Access_Coordinate) is
   Tmp : Access_Coordinate := V1;  -- # stmt
begin
   if V1 /= V2 then                -- # stmt
      Tmp := V1;                   -- # if
      V1  := V2;                   -- # if
      V2  := Tmp;                  -- # if
   end if;
end Swap;
