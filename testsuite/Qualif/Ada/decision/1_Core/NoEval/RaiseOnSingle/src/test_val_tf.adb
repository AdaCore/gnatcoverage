with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_TF is
begin
   Assert (Id (F) = False);
   Assert (Id (T) = True);
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
