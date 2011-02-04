with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_AB_F is
begin
   Assert (F (A => True, B => True, C => False, D => True));
   Assert (not F (A => False, B => True, C => True, D => False));
   Assert (not F (A => True, B => False, C => False, D => False));
end;

--# a1a2.adb
-- /evals/ l! dT-:"C and then D"

