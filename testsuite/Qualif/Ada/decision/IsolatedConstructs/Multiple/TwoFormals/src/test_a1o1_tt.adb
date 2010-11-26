with Support, A1O1; use A1O1, support;

procedure Test_A1O1_TT is
begin
   Assert (F (A => True, B => True) = True);
end;

--# a1o1.adb
--  /valueF/ l! dF-:"A and then B"
--  /evals/  l! dF-:"A and then B" # dF-:"A or else B"
