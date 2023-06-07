with Support, Values; use Support, Values;

procedure Test_B is
begin
   Assert (All_Pos_Some_Neg ((1, 5, 3), (1, 9, -1, 12)));
   Assert (not All_Pos_Some_Neg ((1, 12, 3), (1, 9)));
end Test_B;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## c!:"for all K", d!:"S1(K) > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## c!:"for all K", dF-:"S1(K) > 0"
