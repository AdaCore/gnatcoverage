with Support, A1O2; use A1O2, support;

procedure Test_A1O2_FUPT is
begin
   Assert (F (A => True, B => True, C => True, D => True) = True);
   Assert (F (A => True, B => False, C => True, D => True) = False);
end;

--# a1o2.adb
--  /valueF/ l+ 0
--  /evals/  l! dF-:"C or else D"
