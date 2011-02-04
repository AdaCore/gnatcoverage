with Support, A1O2; use A1O2, support;

procedure Test_A1O2_Full is
begin
   Assert (F (A => True, B => True, C => False, D => True) = True);
   Assert (F (A => False, B => True, C => False, D => False) = False);
end;

--# a1o2.adb
--  /valueF/ l+ 0
--  /evals/  l+ 0
