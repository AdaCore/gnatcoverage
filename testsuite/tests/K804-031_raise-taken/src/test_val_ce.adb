with Args, Val; use Args, Val;

procedure Test_Val_CE is
   X : Boolean;
begin
   X := Ge0 (Num'Last);
end;

--# val.adb
--  /eval/    l! ## d-
--  /true/    l- ## s-
--  /false/   l- ## s-
--  /handler/ l+ ## 0

