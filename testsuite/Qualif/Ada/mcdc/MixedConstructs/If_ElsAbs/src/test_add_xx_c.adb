with Support, Add; use Support, Add;

-- indep(C) wrt "C and then D" controlling the ELSIF part

procedure Test_Add_XX_C is
begin
   Assert -- Right < 0, no overflow
     (Time (20) + Time_Span (-12) = (Valid => True, Value => 8));
   Assert -- Right >= 0, overflow
     (Time (Time'Last) + Time_Span (2) = (Valid => False, Value => 0));
end;

--  A B IF  C D ELSIF
--  F X F   T T T
--  T F F   F X F

--# add.adb
-- /tover0/  l! ## dT-
-- /tover1/  l! ## 0
-- /retp0/   l- ## s-
-- /retp1/   l- ## 0c
-- /tunder0/ l! ## 0
-- /tunder1/ l! ## c!:"Left"
-- /retm0/   l+ ## 0
-- /retm1/   l+ ## 0
-- /fault/   l+ ## 0
