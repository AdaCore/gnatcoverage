with Support, A1O2; use Support, A1O2;

procedure Test_A1O2_TF is
begin
   Assert (F (A => True, B => True, C => False, D => False) = False);
end;

--# a1o2.adb
--  /valueF/ l! dT-
--  /true/  l- s-
--  /false/ l+ 0
--  /evals/  l+;mu=>l! mu=>dF-:"A and then B" # mu=>dT-:"C or else D"
