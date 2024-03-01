with Bar; use Bar;

procedure Test_0 is
begin
   Msg ("");
end Test_0;

--# bar.adb
-- /msg/   l+ ## 0
-- /cond1/ l- ## s-
-- /cond2/ l- ## 0
-- /then/  l- ## s-
-- /else/  l- ## s-
