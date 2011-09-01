with Support, Add; use Support, Add;

-- indep(B) wrt "A and then B" controlling the IF part

procedure Test_Add_B_XX is
begin
   Assert -- Right >= 0, no overflow
     (Time (1) + Time_Span (12) = (Valid => True, Value => 13));
   Assert -- Right >= 0, overflow
     (Time (Time'Last) + Time_Span (2) = (Valid => False, Value => 0));
end;

--  A B IF  C D ELSIF
--  T T T   X X X
--  T F F   F X F

--# add.adb
-- /tover0/  l! c!:"Right"
-- /tover1/  l! 0
-- /retp0/   l+ 0
-- /retp1/   l+ 0
-- /tunder0/ l! dT-
-- /tunder1/ l! 0
-- /retm0/   l- s-
-- /retm1/   l- 0c
-- /fault/   l+ 0
