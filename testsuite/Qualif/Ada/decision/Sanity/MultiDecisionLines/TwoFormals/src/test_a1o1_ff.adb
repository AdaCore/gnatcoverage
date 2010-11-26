with Support, A1O1; use A1O1, support;

procedure Test_A1O1_FF is
begin
   Assert (F (A => False, B => False) = False);
end;

--# a1o1.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B" # dT-:"A or else B"
