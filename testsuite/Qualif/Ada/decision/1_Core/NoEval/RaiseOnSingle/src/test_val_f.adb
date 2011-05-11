with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_F is
begin
   Assert (Id (F) = False);
end;

--# val.adb
--  /eval/  l! d!:"Eval \(A"
--  /true/  l- s-
--  /false/ l+ 0
