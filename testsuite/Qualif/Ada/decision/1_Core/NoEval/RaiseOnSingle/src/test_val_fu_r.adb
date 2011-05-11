with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_FU_R is
   V : Boolean;
begin
   Assert (Id (T) = True);
   Assert (Id (F) = False);
   V := Id (R);
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
