with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_F_R is
   V : Boolean;
begin
   Assert (Id (F) = False);
   V := Id (R);
end;

--# val.adb
--  /eval/  l! d!:"Eval \(A"
--  /true/  l- s-
--  /false/ l+ 0
