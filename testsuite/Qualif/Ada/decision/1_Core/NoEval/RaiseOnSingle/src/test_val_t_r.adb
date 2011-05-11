with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_T_R is
   V : Boolean;
begin
   Assert (Id (T) = True);
   V := Id (R);
end;

--# val.adb
--  /eval/  l! d!:"Eval \(A"
--  /true/  l+ 0
--  /false/ l- s-
