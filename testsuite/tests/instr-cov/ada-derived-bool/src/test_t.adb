with Bar; use Bar;

procedure Test_T is
begin
   Proc ((Val => False), True);
end Test_T;

--# bar.adb
-- /msg/   l+ ## 0
-- /cond1/ l! ## dF-
-- /cond2/ l! ## 0
-- /then/  l+ ## 0
-- /else/  l- ## s-
