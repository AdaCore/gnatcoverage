with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_AB_T is
begin
   Assert (F (A => True, B => True, C => False, D => True));
   Assert (not F (A => False, B => True, C => True, D => False));
   Assert (not F (A => True, B => False, C => True, D => True));
end;

--# a1o2.adb
-- /evals/ l! dF-:"C or else D"

