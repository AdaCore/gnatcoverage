with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_T is
begin
   Process (A => True, B => False, C => True, D => True);
end;

--# a1a2.adb
-- /evals/ l! eT-:"A and then B" # eF-:"C and then D"

