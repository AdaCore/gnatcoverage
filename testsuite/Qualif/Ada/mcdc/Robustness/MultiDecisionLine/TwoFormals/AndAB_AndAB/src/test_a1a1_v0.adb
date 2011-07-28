with Support, A1A1; use Support, A1A1;

-- Exercise single vector #0: A False, B False.
-- Expect and-then False only

procedure Test_A1A1_V0 is
begin
   Process (A => False, B => False);
end;

--# a1a1.adb
-- /evals/  l! eT-:"A and then B," # eT-:"A and then B\)"
