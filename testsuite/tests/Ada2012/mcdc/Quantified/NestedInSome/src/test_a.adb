with Support, Values; use Support, Values;

procedure Test_A is
begin
   Assert (not Some_Pos_Or_Even ((-1, -3, -7)));
   Assert (Some_Pos_Or_Even ((-1, 5)));
end;

--# values.adb
--  /eval/ l! ## c!:"S1(K) mod 2"
