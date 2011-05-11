with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_R is
   B : Boolean;
begin
   B := Id (R);
end;

--# val.adb
--  /eval/  l! d-:"Eval \(A"
--  /true/  l- s-
--  /false/ l- s-
