with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_AB_C is
begin
   Process (A => True, B => True, C => False, D => False);
   Process (A => False, B => True, C => True, D => True); -- SC D
   Process (A => True, B => False, C => False, D => False);
end;

--# a1o2.adb
-- /evals/ l! ## c!:"D"

