with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_AB_C is
begin
   Process (A => True, B => True, C => True, D => True);
   Process (A => False, B => True, C => False, D => True);
   Process (A => True, B => False, C => False, D => False);
end;

--# a1a2.adb
-- /evals/ l! c!:"D"

