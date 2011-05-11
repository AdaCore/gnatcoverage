with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_FX is
begin
   Assert (Andthen (F, F) = False);
   Assert (Andthen (F, T) = False);
end;

--# val.adb
--  /eval/  l! dT-:"Eval \(A"
--  /true/  l- s-
--  /false/ l+ 0
