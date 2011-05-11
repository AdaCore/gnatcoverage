with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_T is
begin
   Assert (Id (T) = True);
end;

--# val.adb
--  /eval/  l! d!:"Eval \(A"
--  /true/  l+ 0
--  /false/ l- s-
