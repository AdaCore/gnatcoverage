with Support, Values; use Support, Values;

procedure Test_B is
   Positives : Sequence := (1, 4, 12, 15, 20);
   Negatives : Sequence := (-2, -4);
begin
   Assert (All_Pos (Positives, Positives));
   Assert (not All_Pos (Positives, Negatives));
end Test_B;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## c!:"for all K", d!:"S1(K) > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## c!:"for all K", dF-:"S1(K) > 0"
