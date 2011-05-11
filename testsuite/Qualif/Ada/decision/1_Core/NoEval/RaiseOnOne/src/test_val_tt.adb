with Support, Args, Val; use Support, Args, Val;

procedure Test_Val_TT is
begin
   Assert (Andthen (T, T) = True);
end;

--# val.adb
--  /eval/  l! dF-:"Eval \(A"
--  /true/  l+ 0
--  /false/ l- s-
