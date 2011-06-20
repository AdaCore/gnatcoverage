with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_CD is
begin
   Process (A => True, B => False, C => True, D => True);
   Process (A => False, B => True, C => True, D => False);
   Process (A => False, B => True, C => False, D => False); -- SC D
end;

--# a1a2.adb
-- /evals/ l! dT-:"A and then B"

