with Support, A1o1; use Support, A1O1;

-- Exercise single vector #3: A True, B True.
-- Expect and-then True only, or-else True only

procedure Test_A1O1_V3 is
begin
   Process (A => True, B => True);
end;

--# a1o1.adb
-- /evals/  l! ## eF-:"A and then B" # eF-:"A or else B"
