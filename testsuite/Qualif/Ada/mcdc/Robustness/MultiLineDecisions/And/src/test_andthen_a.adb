with Support, Andthen; use Support;

procedure Test_Andthen_A is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (False, True) = False);
end;

--# andthen.adb
-- /eval0/  l! 0
-- /eval1/  l! c!:"B"
-- /true/   l+ 0
-- /false/  l+ 0
