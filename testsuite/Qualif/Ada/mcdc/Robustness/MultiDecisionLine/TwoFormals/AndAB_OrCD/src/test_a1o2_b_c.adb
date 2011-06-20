with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_B_C is
begin
   Process (A => True, B => True, C => False, D => False);
   Process (A => True, B => False, C => True, D => False);
end;

--# a1o2.adb
-- /evals/ l! c!:"A" # c!:"D"

