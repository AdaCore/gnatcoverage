with Support, Andthen; use Support;

procedure Test_Andthen_TF is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (False, False) = False);
end;

--# andthen.adb
-- /eval0/  l+ ## 0
-- /eval1/  l+ ## 0c
-- /true/   l+ ## 0
-- /false/  l+ ## 0
