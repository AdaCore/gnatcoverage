with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_A_CD is
begin
   Process (A => True, B => True, C => False, D => False);
   Process (A => False, B => True, C => True, D => False);
   Process (A => False, B => True, C => False, D => True);
end;

--# a1o2.adb
-- /evals/ l! ## c!:"B"
