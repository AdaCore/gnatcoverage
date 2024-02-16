pragma Ada_2012;

with Support, Values; use Support, Values;

procedure Test_T is

   LA : Array_Type :=
     (1 => (X => 2, Count => 0));
begin
   Process (LA, Bump => True);
   Assert (LA(1).Count = 1);
   Assert (LA(1).X = 4);
end;

--# values.adb
--  /stmt/ l+ ## 0
--  /eval/ l! ## dF-
--  /true/ l+ ## 0
