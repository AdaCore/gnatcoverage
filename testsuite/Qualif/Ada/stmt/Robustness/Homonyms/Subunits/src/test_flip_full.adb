with Support, Services; use Support, Services;

procedure Test_Flip_Full is
begin
   Dispatch (Do_Flipb => True, Do_Flipx => True);
end;

--# services.adb services-flipx.adb services-flipb.adb
--  /flipx/ l+ 0
--  /flipb/ l+ 0
