with Support, A1A2; use A1A2, support;

procedure Test_A1A2_FUPT is
begin
   Assert (F (A => True, B => True, C => True, D => True) = True);
   Assert (F (A => True, B => False, C => True, D => True) = False);
end;

--# a1a2.adb
--  /valueF/ l+ 0
--  /evals/  l! dF-:"C and then D"
