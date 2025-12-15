with Proc;

procedure Test_50 is
begin
   Proc (50);
end Test_50;

--# proc.adb
--
-- /before_0/   l+ ## 0
-- /if_0/       l- ## s-
-- /before_100/ l+ ## 0
-- /if_lt_100/  l+ ## 0
-- /final/      l- ## s-
