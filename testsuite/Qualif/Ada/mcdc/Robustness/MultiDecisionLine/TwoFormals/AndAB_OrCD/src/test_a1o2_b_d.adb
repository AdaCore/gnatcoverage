with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_B_D is
begin
   Assert (F (A => True, B => True, C => False, D => False));
   Assert (not F (A => True, B => False, C => False, D => True));
end;

--# a1o2.adb
-- /evals/ l! c!:"A" # c!:"C"

