with Support, Add; use Support, Add;

procedure Test_Add_TT_XX is
begin
   Assert (Time(100) + Time_Span (123) = (Valid => True, Value => 223));
end;

--# add.adb
-- /tover0/  l! dF-
-- /tover1/  l! 0
-- /retp0/   l+ 0
-- /retp1/   l+ 0c
-- /tunder0/ l- s-
-- /tunder1/ l- 0c
-- /retm0/   l- s-
-- /retm1/   l- 0c
-- /fault/   l- s-
