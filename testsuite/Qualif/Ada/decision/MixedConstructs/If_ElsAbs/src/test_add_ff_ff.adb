with Support, Add; use Support, Add;

procedure Test_Add_FF_FF is
begin
   Assert (Time (123) + Time_Span (-124) = (False, Time (0)));
end;

--# add.adb
-- /tover0/  l! dT-
-- /tover1/  l! 0
-- /retplus/ l- s-
-- /tunder0/ l! dT-
-- /tunder1/ l! 0
-- /retmin/  l- s-
-- /fault/   l+ 0
