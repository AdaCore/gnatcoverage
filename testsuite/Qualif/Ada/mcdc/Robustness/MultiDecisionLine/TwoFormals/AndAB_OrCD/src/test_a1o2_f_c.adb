with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_F_C is
begin
   Process (A => True, B => False, C => False, D => False);
   Process (A => False, B => True, C => True, D => False);
end;

--# a1o2.adb
-- /evals/ l! ## eT-:"A and then B" # c!:"D"
