with Proc;

procedure Test_Full is
begin
   Proc (False, False);
   Proc (False, True);
   Proc (True, False);
end Test_Full;

--# proc.adb
--
-- /stmt/     l+ ## 0
-- /decision/ l+ ## 0
