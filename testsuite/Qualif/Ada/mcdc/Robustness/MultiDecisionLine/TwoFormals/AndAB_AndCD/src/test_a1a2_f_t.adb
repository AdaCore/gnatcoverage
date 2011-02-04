with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_T is
begin
   Assert (not F (A => True, B => False, C => True, D => True));
end;

--# a1a2.adb
-- /evals/ l! dT-:"A and then B" # dF-:"C and then D"

