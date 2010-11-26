with Support, A1A2; use A1A2, support;

procedure Test_A1A2_FUPF is
begin
   Assert (F (A => True, B => True, C => True, D => False) = False);
   Assert (F (A => True, B => False, C => False, D => True) = False);
end;

--# a1a2.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"C and then D"
