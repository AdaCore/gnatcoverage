with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_AB_F is
begin
   Process (A => True, B => True, C => False, D => True);
   Process (A => False, B => True, C => True, D => False);
   Process (A => True, B => False, C => False, D => False);
end;

--# a1a2.adb
-- /evals/ l! ## eT-:"C and then D"
