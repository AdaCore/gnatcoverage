with Support, Values; use Support, Values;

procedure Test_A is
   Positives : Sequence := (1, 4, 12, 15, 20);
   Negatives : Sequence := (-2, -4);
begin
   Assert (All_Pos (Positives, Positives));
   Assert (not All_Pos (Negatives, Positives));
end Test_A;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## c!:"for all E", d!:"E > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## c!:"for all E", dF-:"E > 0"
