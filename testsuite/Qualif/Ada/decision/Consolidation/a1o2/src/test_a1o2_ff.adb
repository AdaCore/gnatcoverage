with Support, A1O2; use Support, A1O2;

procedure Test_A1O2_FF is
begin
   Assert (F (A => False, B => True, C => False, D => False) = False);
end;

--# a1o2.adb
--  /valueF/ l! dT-
--  /evals/  l! dT-:"A and then B" # dT-:"C or else D"
