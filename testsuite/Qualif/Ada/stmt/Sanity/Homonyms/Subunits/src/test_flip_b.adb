with Support, Services; use Support, Services;

procedure Test_Flip_B is
begin
   Dispatch (Do_Flipb => True, Do_Flipx => False);
end;

--# services.adb services-flipx.adb services-flipb.adb
--  /flipx/ l- s-
--  /flipb/ l+ 0
