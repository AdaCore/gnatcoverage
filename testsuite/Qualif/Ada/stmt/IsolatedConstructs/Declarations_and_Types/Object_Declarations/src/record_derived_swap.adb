--  This procedure contains declaration of an object of a record type without
--  explicit initialization expression, but the record components have default
--  initialization.
with Decls_Support; use Decls_Support;
with Support;       use Support;
procedure Record_Derived_Swap (V1, V2 : in out Derived_Discrete_Coordinate) is
   Tmp : Derived_Discrete_Coordinate;  -- # stmt
begin
   if V1 /= V2 then                    -- # stmt
      Tmp := V1;                       -- # if
      V1  := V2;                       -- # if
      V2  := Tmp;                      -- # if
   end if;
end Record_Derived_Swap;

