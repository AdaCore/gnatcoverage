with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_F_F is
begin
   Process (A => True, B => False, C => True, D => False);
   Process (A => False, B => False, C => True, D => False);
   Process (A => False, B => True, C => True, D => False);

   Process (A => False, B => True, C => False, D => True);
end;

--# a1a2.adb
-- /evals/ l! ## eT-:"A and then B" # eT-:"C and then D"
