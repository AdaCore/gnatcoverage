with Support, A1O1; use A1O1, support;

procedure Test_A1O1_TF is
begin
   Assert (F (A => True, B => False) = False);
end;

--# a1o1.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B" # dF-:"A or else B"
