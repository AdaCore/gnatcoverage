with Support, Values; use Support, Values;

procedure Test_A is
begin
   Assert (All_Pos_Some_Neg ((1, 5, 3), (1, 9, -1, 12)));
   Assert (not All_Pos_Some_Neg ((1, -5, 3), (1, 9, -1, 12)));
end Test_A;

--# values.adb
--  /eval/ l! ## c!:"for some E"
