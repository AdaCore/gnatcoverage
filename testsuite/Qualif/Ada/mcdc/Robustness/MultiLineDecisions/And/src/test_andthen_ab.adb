with Support, Andthen; use Support;

procedure Test_Andthen_AB is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (True, False) = False);
   Assert (Andthen (False, True) = False);
end;

--# andthen.adb
-- /eval0/  l+ ## 0
-- /eval1/  l+ ## 0
-- /true/   l+ ## 0
-- /false/  l+ ## 0
