with Support, A1A1; use Support, A1A1;

-- Exercise single vector #2: A True, B False.
-- Expect and-then False only

procedure Test_A1A1_V2 is
begin
   Assert (not F (A => True, B => False));
end;

--# a1a1.adb
-- /evals/  l! dT-:"A and then B," # dT-:"A and then B\)"

