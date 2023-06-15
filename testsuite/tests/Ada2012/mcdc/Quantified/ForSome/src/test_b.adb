with Support, Values; use Support, Values;

procedure Test_B is
   Positives : Sequence := (1, 4, 12, 15, 20);
   Negatives : Sequence := (-2, -4);
begin
   Assert (not Some_Pos (Negatives, Negatives));
   Assert (Some_Pos (Negatives, Positives));
end Test_B;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## c!:"for some K", d!:"S1(K) > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## c!:"for some K", dT-:"S1(K) > 0"
