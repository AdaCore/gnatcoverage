with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_D is
begin
   Assert (not F (A => True, B => False, C => True, D => True));
   Assert (not F (A => False, B => True, C => True, D => False));
end;

--# a1a2.adb
-- /evals/ l! dT-:"A and then B" # c!:"C"

