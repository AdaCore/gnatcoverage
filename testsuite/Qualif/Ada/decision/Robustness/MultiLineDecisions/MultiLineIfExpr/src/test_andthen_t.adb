with Support, And_Then; use Support;

procedure Test_Andthen_T is
begin
   Assert (And_Then (True, True) = True);
end;

--# and_then.adb
-- /eval0/  l! dF-
-- /eval1/  l! 0
-- /true/   l+ 0
-- /false/  l- s-
