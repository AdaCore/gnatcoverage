with Bar; use Bar;

procedure Test_F is
begin
   Proc ((Val => True), False);
end Test_F;

--# bar.adb
-- /msg/   l+ ## 0
-- /cond1/ l! ## dT-
-- /cond2/ l! ## 0
-- /then/  l- ## s-
-- /else/  l+ ## 0
