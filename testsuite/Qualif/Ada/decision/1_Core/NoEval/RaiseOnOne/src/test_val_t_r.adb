with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_T_R is
begin
   Assert (Andthen (T, T) = True);
   
   Assert (Andthen (R, T) = False);
end;

--# val.adb
--  /eval/  l! dF-:"Eval \(A"
--  /true/  l+ 0
--  /false/ l- s-
