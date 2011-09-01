with Support, Add; use Support, Add;

procedure Test_Add_FX_TT is
begin
   Assert (Time(100) + Time_Span (-10) = (Valid => True, Value => 90));
end;

--# add.adb
-- /tover0/  l! dT-
-- /tover1/  l! 0
-- /retp0/   l- s-
-- /retp1/   l- 0c
-- /tunder0/ l! dF-
-- /tunder1/ l! 0
-- /retm0/   l+ 0
-- /retm1/   l+ 0c
-- /fault/   l- s-
