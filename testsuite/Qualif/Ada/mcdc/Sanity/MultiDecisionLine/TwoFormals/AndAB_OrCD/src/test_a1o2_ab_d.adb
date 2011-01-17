with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_AB_D is
begin
   Assert (F (A => True, B => True, C => False, D => False));
   Assert (not F (A => False, B => True, C => False, D => True));
   Assert (not F (A => True, B => False, C => False, D => False));
end;

--# a1o2.adb
-- /evals/ l! c!:"C"

