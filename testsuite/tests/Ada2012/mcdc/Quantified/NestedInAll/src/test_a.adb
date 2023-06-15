with Support, Values; use Support, Values;

procedure Test_A is
begin
   Assert (All_Pos_And_Even ((2, 8)));
   Assert (not All_Pos_And_Even ((28, 8, -14)));
end Test_A;

--# values.adb
--  /eval/ l! ## c!:"X mod 2"
