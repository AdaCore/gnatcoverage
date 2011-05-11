with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_RX is
   V : Boolean;
begin
   V := Andthen (R, T);
end;

--# val.adb
--  /eval/  l! d-:"Eval \(A"
--  /true/  l- s-
--  /false/ l- s-
