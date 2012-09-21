with Support, A1A1; use Support, A1A1;

-- Exercise single vector #2: A True, B False.
-- Expect and-then False only

procedure Test_A1A1_V2 is
begin
   Process (A => True, B => False);
end;

--# a1a1.adb
-- /evals/  l! ## eT-:"A and then B," # eT-:"A and then B\)"

