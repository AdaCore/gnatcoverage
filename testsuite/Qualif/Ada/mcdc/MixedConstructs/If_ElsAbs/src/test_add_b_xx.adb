with Support, Add; use Support, Add;

procedure Test_Add_A_XX is
begin
   Assert (Time (1) + Time_Span (12) = (Valid => True, Value => 23));
   Assert (Time (1) + Time_Span (-2) = (Valid => False, Value => 0));
end;

--# add.adb
-- /tover0/  l! c!:"Right"
-- /tover1/  l! 0
-- /retp0/   l+ 0
-- /retp1/   l+ 0
-- /tunder0/ l! dT-
-- /tunder1/ l! 0
-- /retm0/   l- s-
-- /retm1/   l- 0
-- /fault/   l+ 0
