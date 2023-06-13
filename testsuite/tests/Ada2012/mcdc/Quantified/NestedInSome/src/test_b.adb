with Support, Values; use Support, Values;

procedure Test_B is
begin
   Assert (not Some_Pos_Or_Even ((-1, -3, -7)));
   Assert (Some_Pos_Or_Even ((-1, -8)));
end Test_B;

--# values.adb
--  /eval/ l! ## c!:"S1(K) > 0"
