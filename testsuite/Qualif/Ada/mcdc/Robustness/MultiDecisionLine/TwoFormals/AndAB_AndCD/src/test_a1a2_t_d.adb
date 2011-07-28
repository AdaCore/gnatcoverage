with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_T_D is
begin
   Process (A => True, B => True, C => True, D => True);
   Process (A => True, B => True, C => True, D => False);
end;

--# a1a2.adb
-- /evals/ l! eF-:"A and then B" # c!:"C"

