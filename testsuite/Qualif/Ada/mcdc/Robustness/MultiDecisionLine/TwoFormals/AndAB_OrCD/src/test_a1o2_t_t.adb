with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_T_T is
begin
   Process (A => True, B => True, C => True, D => False);
end;

--# a1o2.adb
-- /evals/ l! dF-:"A and then B" # dF-:"C or else D"

