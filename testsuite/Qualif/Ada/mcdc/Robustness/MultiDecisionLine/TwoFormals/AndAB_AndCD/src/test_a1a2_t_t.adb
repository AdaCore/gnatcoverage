with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_T_T is
begin
   Process (A => True, B => True, C => True, D => True);
end;

--# a1a2.adb
-- /evals/ l! dF-:"A and then B" # dF-:"C and then D"

