with A1A2, Support; use A1A2, Support;

procedure Test_A1A2_A_F is
begin
   Process (A => True, B => True, C => False, D => True);
   Process (A => False, B => True, C => True, D => False);
end;

--# a1a2.adb
-- /evals/ l! c!:"B" # dT-:"C and then D"

