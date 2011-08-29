with Support, Silent_Last_Chance, Value; use Support, Value;

procedure Test_Value_TF is
begin
   Assert (F (True) = True);
   Assert (F (False) = False);
end;

--# value.adb
-- /eval/    l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnVal/   l+ 0
