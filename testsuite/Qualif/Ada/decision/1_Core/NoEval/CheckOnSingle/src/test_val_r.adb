with Silent_Last_Chance, Support, Args, Val; use Support, Args, Val;

procedure Test_Val_R is
   V : Boolean;
begin
   V := GE0(Num'Last);
end;

--# val.adb
--  /eval/  l! d-:"Id"
--  /true/  l- s-
--  /false/ l- s-
