with Support, A1O2; use A1O2, support;

procedure Test_A1O2_PTPT is
begin
   Assert (F (A => True, B => True, C => False, D => True) = True);
end;

--# a1o2.adb
--  /valueF/ l! dF-:"A and then B"
--  /evals/  l! dF-:"A and then B" # dF-:"C or else D"
