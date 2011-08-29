with Support, Value, Silent_Last_Chance; use Support, Value;

procedure Test_Value_F is
begin
   Assert (F (False) = False);
end;

--# value.adb
-- /eval/        l! o!
-- /returnTrue/  l- s-
-- /returnFalse/ l- s-
-- /returnVal/   l- s-
