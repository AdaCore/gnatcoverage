with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_AB_T is
begin
   Process (A => True, B => True, C => False, D => True);
   Process (A => False, B => True, C => True, D => False);
   Process (A => True, B => False, C => True, D => True);
end;

--# a1o2.adb
-- /evals/ l! ## eF-:"C or else D"
