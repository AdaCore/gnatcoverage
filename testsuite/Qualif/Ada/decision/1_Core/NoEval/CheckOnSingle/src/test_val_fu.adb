with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_FU is
begin
   Assert (GE0(0) = True);
   Assert (GE0(-1) = False);
end;

--# val.adb
--  /eval/  l+ 0
--  /true/  l+ 0
--  /false/ l+ 0
