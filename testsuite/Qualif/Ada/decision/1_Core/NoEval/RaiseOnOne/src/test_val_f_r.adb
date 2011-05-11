with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_F_R is
begin
   Assert (Andthen (F, F) = False);
   Assert (Andthen (F, T) = False);
   
   Assert (Andthen (R, T) = False);
end;

--# val.adb
--  /eval/  l! dT-:"Eval \(A"
--  /true/  l- s-
--  /false/ l+ 0
