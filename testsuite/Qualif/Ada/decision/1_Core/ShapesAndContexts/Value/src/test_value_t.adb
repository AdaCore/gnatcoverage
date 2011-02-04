with Support, Value; use Support, Value;

procedure Test_Value_T is
begin
   Assert (F (True) = True);
end;

--# value.adb
-- /evaluate/    l! d!
-- /returnTrue/  l+ 0
-- /returnFalse/ l- s-
-- /returnVal/   l+ 0
