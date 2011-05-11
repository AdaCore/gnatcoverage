with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_T is
begin
   Assert (GE0(0) = True);
end;

--# val.adb
--  /eval/  l! d!:"Id"
--  /true/  l+ 0
--  /false/ l- s-
