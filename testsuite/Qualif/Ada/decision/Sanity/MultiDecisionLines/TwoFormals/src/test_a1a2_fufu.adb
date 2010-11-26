with Support, A1A2; use A1A2, support;

procedure Test_A1A2_FUFU is
begin
   Assert (F (A => True, B => True, C => True, D => True) = True);
   Assert (F (A => True, B => True, C => True, D => False) = False);
   Assert (F (A => False, B => True, C => True, D => False) = False);
end;

--# a1a2.adb
--  /valueF/ l+ 0
--  /evals/  l+ 0
