with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_B_T is
begin
   Process (A => True, B => True, C => True, D => True);
   Process (A => True, B => False, C => True, D => True);
end;

--# a1a2.adb
-- /evals/ l! ## c!:"A" # eF-:"C and then D"
