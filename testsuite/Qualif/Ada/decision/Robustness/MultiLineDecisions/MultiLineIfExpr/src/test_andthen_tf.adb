with Support, And_Then; use Support;

procedure Test_Andthen_TF is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, False) = False);
end;

--# and_then.adb
-- /eval0/  l+ 0
-- /eval1/  l+ 0
-- /true/   l+ 0
-- /false/  l+ 0
