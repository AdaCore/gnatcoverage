with Support, A1A1; use A1A1, support;

procedure Test_A1A1_PF is
begin
   Assert (F (A => True, B => False) = False);
   Assert (F (A => False, B => True) = False);
   Assert (F (A => False, B => False) = False);
end;

--# a1a1.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B," # dT-:"A and then B\)"
