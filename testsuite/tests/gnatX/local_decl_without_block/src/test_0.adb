with Proc;

procedure Test_0 is
begin
   Proc (0);
end Test_0;

--# proc.adb
--
-- /before_0/   l+ ## 0
-- /if_0/       l+ ## 0
-- /before_100/ l- ## s-
-- /if_lt_100/  l- ## s-
-- /final/      l- ## s-
