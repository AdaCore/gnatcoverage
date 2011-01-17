with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_T_D is
begin
   Assert (F (A => True, B => True, C => False, D => False));
   Assert (F (A => True, B => True, C => False, D => True));
end;

--# a1o2.adb
-- /evals/ l! dF-:"A and then B" # c!:"C"

