with Support, Values; use Support, Values;

procedure Test_B is
begin
   Assert (All_Pos_And_Even ((2, 8)));
   Assert (not All_Pos_And_Even ((28, 8, 7)));
end Test_B;

--# values.adb
--  /eval/ l! ## c!:"X > 0"
