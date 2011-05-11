with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_F is
begin
   Assert (GE0(-1) = False);
end;

--# val.adb
--  /eval/  l! d!:"Id"
--  /true/  l- s-
--  /false/ l+ 0
