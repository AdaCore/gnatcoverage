with Bar; use Bar;

procedure Test_TF is
begin
   Proc ((Val => True), True);
   Proc ((Val => False), True);
end Test_TF;

--# bar.adb
-- /msg/   l+ ## 0
-- /cond1/ l+ ## 0
-- /cond2/ l+ ## 0
-- /then/  l+ ## 0
-- /else/  l+ ## 0
