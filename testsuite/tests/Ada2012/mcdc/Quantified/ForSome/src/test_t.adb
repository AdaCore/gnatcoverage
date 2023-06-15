with Support, Values; use Support, Values;

procedure Test_T is
begin
   Assert (Some_Pos ((-1, -2), (3, -8)));
end Test_T;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## eF-, d!:"S1(K) > 0", d!:"E > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## eF-, dT-:"S1(K) > 0", dF-:"E > 0"
