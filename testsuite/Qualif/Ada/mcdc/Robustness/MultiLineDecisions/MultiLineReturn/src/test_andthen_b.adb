with Support, And_Then; use Support;

procedure Test_Andthen_B is
begin
   Assert (And_Then (True, True) = True);
   Assert (And_Then (True, False) = False);
end;

--# and_then.adb
-- /return/  l! c!:"A"
-- /andthen/ l! 0
