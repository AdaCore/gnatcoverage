with Support, A1o1; use Support, A1O1;

-- Exercise single vector #0: A False, B False.
-- Expect and-then False only, or-else False only

procedure Test_A1O1_V0 is
begin
   Process (A => False, B => False);
end;

--# a1o1.adb
-- /evals/  l! eT-:"A and then B" # eT-:"A or else B"
