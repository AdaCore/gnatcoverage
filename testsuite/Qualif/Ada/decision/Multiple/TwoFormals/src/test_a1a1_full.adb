with Support, A1A1; use A1A1, support;

procedure Test_A1A1_Full is
begin
   Assert (F (A => True, B => True) = True);
   Assert (F (A => False, B => False) = False);
end;

--# a1a1.adb
--  /valueF/ l+ 0
--  /evals/  l+ 0
