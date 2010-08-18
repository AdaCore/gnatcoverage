with Support, A1A1; use A1A1, support;

procedure Test_A1A1_TF is
begin
   Assert (F (A => True, B => False) = False);
end;

--# a1a1.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B," # dT-:"A and then B\)"
