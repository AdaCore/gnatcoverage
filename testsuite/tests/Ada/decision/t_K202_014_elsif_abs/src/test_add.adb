with Support, Add; use Support, Add;

procedure Test_Add is
begin
   Assert (Time (123) + Time_Span (123) = (True, Time (246)));
   Assert (Time (124) + Time_Span (-123) = (True, Time (1)));
   Assert (Time (123) + Time_Span (-124) = (False, Time (0)));
end;

--# add.adb
-- /tover0/  l+ ## 0
-- /tover1/  l+ ## 0
-- /retplus/ l+ ## 0
-- /tunder/  l+ ## 0
-- /retmin/  l+ ## 0
-- /fault/   l+ ## 0
