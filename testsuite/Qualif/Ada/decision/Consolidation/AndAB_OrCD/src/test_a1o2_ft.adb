with Support, A1O2; use Support, A1O2;

procedure Test_A1O2_FT is
begin
   Assert (F (A => False, B => False, C => True, D => True) = False);
end;

--# a1o2.adb
--  /valueF/ l! dT-
--  /true/  l- s-
--  /false/ l+ 0
--  /evals/  l+;mu=>l! mu=>dT-:"A and then B" # mu=>dF-:"C or else D"
