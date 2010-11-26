with Support, And_Then; use Support;

procedure Test_Andthen_TF is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (False, False) = False);
end;

--# and_then.adb
-- /return/  l+ 0
-- /andthen/ l+ 0
