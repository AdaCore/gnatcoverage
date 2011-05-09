with Support, And_Then; use Support;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, True)  = False);
   Assert (And_Then (True , False) = False);
   Assert (And_Then (False, False) = False);
end;

--# and_then.adb
-- /eval0/  l! dT-
-- /eval1/  l! 0
-- /true/   l- s-
-- /false/  l+ 0
