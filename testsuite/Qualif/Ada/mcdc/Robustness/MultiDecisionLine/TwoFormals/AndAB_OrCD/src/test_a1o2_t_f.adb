with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_T_F is
begin
   Process (A => True, B => True, C => False, D => False);
end;

--# a1o2.adb
-- /evals/ l! dF-:"A and then B" # dT-:"C or else D"

