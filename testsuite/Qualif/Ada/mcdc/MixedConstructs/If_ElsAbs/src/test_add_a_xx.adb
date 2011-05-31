with Support, Add; use Support, Add;

procedure Test_Add_A_XX is
begin
   Assert (Time (12) + Time_Span (12) = (Valid => True, Value => 24));
   Assert (Time (12) + Time_Span (-2) = (Valid => True, Value => 10));
end;

--# add.adb
-- /tover0/  l! 0
-- /tover1/  l! c!:"Uint_64(Time.Last)"
-- /retp0/   l+ 0
-- /retp1/   l+ 0
-- /tunder0/ l! dF-
-- /tunder1/ l! 0
-- /retm0/   l+ 0
-- /retm1/   l+ 0
-- /fault/   l- s-
