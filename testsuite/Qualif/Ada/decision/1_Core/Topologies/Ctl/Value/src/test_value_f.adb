with Support, Value; use Support, Value;

procedure Test_Value_F is
begin
   Assert (F (False) = False);
end;

--# value.adb
-- /evaluate/    l! d!
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0
-- /returnVal/   l+ 0
