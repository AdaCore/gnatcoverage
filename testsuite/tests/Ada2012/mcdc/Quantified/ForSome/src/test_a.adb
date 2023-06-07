with Support, Values; use Support, Values;

procedure Test_A is
   Positives : Sequence := (1, 4, 12, 15, 20);
   Negatives : Sequence := (-2, -4);
begin
   Assert (not Some_Pos (Negatives, Negatives));
   Assert (Some_Pos (Positives, Negatives));
end Test_A;

--# values.adb
-- %opts: --trace-mode=bin
--  /eval/ l! ## c!:"for some E", d!:"E > 0"
-- %opts: --trace-mode=src
--  /eval/ l! ## c!:"for some E", dT-:"E > 0"
