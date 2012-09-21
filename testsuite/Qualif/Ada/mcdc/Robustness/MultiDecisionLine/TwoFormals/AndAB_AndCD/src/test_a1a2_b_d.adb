with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_B_D is
begin
   Process (A => True, B => True, C => True, D => True);
   Process (A => True, B => False, C => True, D => False);
end;

--# a1a2.adb
-- /evals/ l! ## c!:"A" # c!:"C"

