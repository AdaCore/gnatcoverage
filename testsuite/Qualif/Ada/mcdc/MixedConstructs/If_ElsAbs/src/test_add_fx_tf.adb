with Support, Add; use Support, Add;

procedure Test_Add_FX_TF is
begin
   Assert (Time (123) + Time_Span (-124) = (Valid => False, Value => 0));
end;

--# add.adb
-- /tover0/  l! ## dT-
-- /tover1/  l! ## 0
-- /retp0/   l- ## s-
-- /retp1/   l- ## 0c
-- /tunder0/ l! ## dT-
-- /tunder1/ l! ## 0
-- /retm0/   l- ## s-
-- /retm1/   l- ## 0c
-- /fault/   l+ ## 0
