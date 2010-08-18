with Support, A1O2; use A1O2, support;

procedure Test_A1O2_PFPF is
begin
   Assert (F (A => True, B => False, C => False, D => False) = False);
end;

--# a1o2.adb
--  /valueF/ l! dT-:"A and then B"
--  /evals/  l! dT-:"A and then B" # dT-:"C or else D"
