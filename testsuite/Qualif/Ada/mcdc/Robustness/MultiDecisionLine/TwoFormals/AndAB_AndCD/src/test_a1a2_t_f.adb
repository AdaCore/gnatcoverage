with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_T_F is
begin
   Process (A => True, B => True, C => False, D => True);
end;

--# a1a2.adb
-- /evals/ l! ## eF-:"A and then B" # eT-:"C and then D"

