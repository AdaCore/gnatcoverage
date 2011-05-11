with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_TF is
begin
   Assert (Andthen (T, F) = False);
end;

--# val.adb
--  /eval/  l! dT-:"Eval \(A"
--  /true/  l- s-
--  /false/ l+ 0
