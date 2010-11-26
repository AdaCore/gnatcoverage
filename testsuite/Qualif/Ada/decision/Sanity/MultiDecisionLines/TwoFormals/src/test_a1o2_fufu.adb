with Support, A1O2; use A1O2, support;

procedure Test_A1O2_FUFU is
begin
   Assert (F (A => True, B => True, C => False, D => False) = False);
   Assert (F (A => False, B => False, C => True, D => True) = False);
end;

--# a1o2.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l+ 0
