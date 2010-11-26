with Support, A1A2; use A1A2, support;

procedure Test_A1A2_PFFU is
begin
   Assert (F (A => False, B => True, C => True, D => True) = False);
   Assert (F (A => True, B => False, C => False, D => True) = False);
end;

--# a1a2.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B"
