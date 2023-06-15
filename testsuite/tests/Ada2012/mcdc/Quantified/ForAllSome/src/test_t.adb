with Support, Values; use Support, Values;

procedure Test_T is
begin
   Assert (All_Pos_Some_Neg ((1, 5, 3), (1, 9, -1, 12)));
end Test_T;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## eF-, d!:"S1(K) > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## eF-, dF-:"S1(K) > 0"
