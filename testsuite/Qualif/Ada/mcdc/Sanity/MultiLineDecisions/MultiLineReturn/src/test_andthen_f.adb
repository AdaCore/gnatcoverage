with Support, And_Then; use Support;

procedure Test_Andthen_F is
begin
   Assert (And_Then (False, True)  = False);
   Assert (And_Then (True , False) = False);
   Assert (And_Then (False, False) = False);
end;

--# and_then.adb
-- /return/  l! dT-
-- /andthen/ l! 0
