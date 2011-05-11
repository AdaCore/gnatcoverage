with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_FU_R is
begin
   Assert (Andthen (T, T) = True);
   Assert (Andthen (T, F) = False);
   
   Assert (Andthen (R, T) = False);
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
