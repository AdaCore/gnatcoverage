with Support, A1A1; use Support, A1A1;

-- Exercise single vector #1: A False, B True.
-- Expect and-then False only

procedure Test_A1A1_V1 is
begin
   Process (A => False, B => True);
end;

--# a1a1.adb
-- /evals/  l! ## eT-:"A and then B," # eT-:"A and then B\)"
