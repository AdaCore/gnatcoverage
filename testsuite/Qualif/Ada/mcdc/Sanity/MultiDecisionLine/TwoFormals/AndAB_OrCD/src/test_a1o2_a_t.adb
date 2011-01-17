with A1O2, Support; use A1O2, Support;

procedure Test_A1O2_A_T is
begin
   Assert (F (A => True, B => True, C => True, D => False));
   Assert (not F (A => False, B => True, C => False, D => True));
end;

--# a1o2.adb
-- /evals/ l! c!:"B" # dF-:"C or else D"

