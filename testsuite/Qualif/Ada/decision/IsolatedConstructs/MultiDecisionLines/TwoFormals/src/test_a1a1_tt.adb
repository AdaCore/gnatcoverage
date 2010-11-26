with Support, A1A1; use A1A1, support;

procedure Test_A1A1_TT is
begin
   Assert (F (A => True, B => True) = True);
end;

--# a1a1.adb
--  /valueF/ l! dF-:"A and then B"
--  /evals/  l! dF-:"A and then B," # dF-:"A and then B\)"
