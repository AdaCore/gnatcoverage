with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_C is
begin
   Process (A => True, B => False, C => True, D => True);
   Process (A => False, B => True, C => False, D => True);
end;

--# a1a2.adb
-- /evals/ l! dT-:"A and then B" # c!:"D"

