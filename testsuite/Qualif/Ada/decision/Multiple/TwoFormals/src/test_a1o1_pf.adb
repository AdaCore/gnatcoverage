with Support, A1O1; use A1O1, support;

procedure Test_A1O1_PF is
begin
   Assert (F (A => True, B => False) = False);
   Assert (F (A => False, B => True) = False);
   Assert (F (A => False, B => False) = False);
end;

--# a1o1.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B" # dT-:"A and then B"
