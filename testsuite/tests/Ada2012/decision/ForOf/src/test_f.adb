pragma Ada_2012;

with Support, Values; use Support, Values;

procedure Test_F is

   LA : Array_Type :=
     (1 => (X => 2, Count => 0));
begin
   Process (LA, Bump => False);
   Assert (LA(1).Count = 0);
   Assert (LA(1).X = 4);
end;

--# values.adb
--  /stmt/ l+ ## 0
--  /eval/ l! ## dT-
--  /true/ l- ## s-
