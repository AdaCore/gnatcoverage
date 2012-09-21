with Support, Add; use Support, Add;

-- indep(A) and indep(B) wrt "A and then B" controlling the IF part

procedure Test_Add_AB_XX is
begin
   Assert -- Right >= 0, no overflow
     (Time (12) + Time_Span (12) = (Valid => True, Value => 24));
   Assert -- Right >= 0, overflow
     (Time (Time'Last) + Time_Span (12) = (Valid => False, Value => 0));
   Assert -- Right < 0, no overflow
     (Time (12) + Time_Span (-2) = (Valid => True, Value => 10));
end;

--  A B IF  C D ELSIF
--  T T T   X X X
--  T F F   F X F
--  F X F   T T T

--# add.adb
-- /tover0/  l+ ## 0
-- /tover1/  l+ ## 0
-- /retp0/   l+ ## 0
-- /retp1/   l+ ## 0
-- /tunder0/ l! ## 0
-- /tunder1/ l! ## c!:"Left"
-- /retm0/   l+ ## 0
-- /retm1/   l+ ## 0
-- /fault/   l+ ## 0
