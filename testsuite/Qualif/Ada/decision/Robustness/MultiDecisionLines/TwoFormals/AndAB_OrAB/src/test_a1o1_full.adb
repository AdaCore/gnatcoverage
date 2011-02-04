with Support, A1O1; use A1O1, support;

procedure Test_A1O1_Full is
begin
   Assert (F (A => True, B => True) = True);
   Assert (F (A => False, B => False) = False);
end;

--# a1o1.adb
--  /valueF/ l+ 0
--  /evals/  l+ 0
