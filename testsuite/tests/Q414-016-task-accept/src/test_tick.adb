with Q;

procedure Test_Tick is
begin
   Q.Tryme;
end Test_Tick;

--# q.adb
-- /start_accept/ l- ## s-
-- /start_stmt/   l- ## s-
-- /tick_accept/  l+ ## 0
-- /tick_if/      l+ ## 0
-- /tick_stmt/    l- ## s-
