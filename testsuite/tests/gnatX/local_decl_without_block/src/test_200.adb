with Proc;

procedure Test_200 is
begin
   Proc (200);
end Test_200;

--# proc.adb
--
-- /before_0/   l+ ## 0
-- /if_0/       l- ## s-
-- /before_100/ l+ ## 0
-- /if_lt_100/  l- ## s-
-- /final/      l+ ## 0
