with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_T_C is
begin
   Process (A => True, B => True, C => False, D => False);
   Process (A => True, B => True, C => True, D => True); -- SC D
end;

--# a1o2.adb
-- /evals/ l! ## eF-:"A and then B" # c!:"D"

