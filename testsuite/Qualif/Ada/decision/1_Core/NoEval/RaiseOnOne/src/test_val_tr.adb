with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_TR is
   V : Boolean;
begin
   V := Andthen (T, R);
end;

--# val.adb
--  /eval/  l! d-:"Eval \(A"
--  /true/  l- s-
--  /false/ l- s-
