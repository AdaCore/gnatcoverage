with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_F_T is
begin
   Assert (not F (A => True, B => False, C => True, D => False));
   Assert (not F (A => True, B => False, C => False, D => True));
end;

--# a1o2.adb
-- /evals/ l! dT-:"A and then B" # dF-:"C or else D"

