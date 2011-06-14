with Support, Andthen; use Support;

procedure Test_Andthen_B is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (True, False) = False);
end;

--# andthen.adb
-- /eval0/  l! c!:"A"
-- /eval1/  l! 0
-- /true/   l+ 0
-- /false/  l+ 0
