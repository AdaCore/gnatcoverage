with Support, A1O2; use Support, A1O2;

procedure Test_A1O2_TT is
begin
   Assert (F (A => True, B => True, C => True, D => False) = True);
end;

--# a1o2.adb
--  /valueF/ l! dF-
--  /evals/  l! dF-:"A and then B" # dF-:"C or else D"
