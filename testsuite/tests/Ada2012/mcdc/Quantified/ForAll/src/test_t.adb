with Support, Values; use Support, Values;

procedure Test_T is
   S1 : Sequence  := (1, 4, 12, 15, 20);
   S2 : Sequence  := (1, 15, 13);
begin
   Assert (All_Pos (S1, S2));
end Test_T;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## eF-, d!:"S1(K) > 0", d!:"E > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## eF-, dF-:"S1(K) > 0", dF-:"E > 0"
