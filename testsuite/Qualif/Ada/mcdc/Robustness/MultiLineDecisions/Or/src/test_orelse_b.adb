with Support, Orelse; use Support;

procedure Test_Orelse_B is
begin
   Assert (Orelse (False, False) = False);
   Assert (Orelse (False, True) = True);
end;

--# orelse.adb
-- /eval0/  l! ## c!:"A"
-- /eval1/  l! ## 0
-- /true/   l+ ## 0
-- /false/  l+ ## 0
