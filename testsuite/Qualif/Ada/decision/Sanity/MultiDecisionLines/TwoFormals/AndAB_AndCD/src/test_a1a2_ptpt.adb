with Support, A1A2; use A1A2, support;

procedure Test_A1A2_PTPT is
begin
   Assert (F (A => True, B => True, C => True, D => True) = True);
end;

--# a1a2.adb
--  /valueF/ l! dF-:"A and then B"
--  /evals/  l! dF-:"A and then B" # dF-:"C and then D"
