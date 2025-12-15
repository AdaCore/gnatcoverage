with Proc;

procedure Test_Unused is
begin
   null;
end Test_Unused;

--# proc.adb
--
-- /before_0/   l- ## s-
-- /if_0/       l- ## s-
-- /before_100/ l- ## s-
-- /if_lt_100/  l- ## s-
-- /final/      l- ## s-
