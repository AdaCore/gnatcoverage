with Support, Services; use Support, Services;

procedure Test_Flip_X is
begin
   Dispatch (Do_Flipb => False, Do_Flipx => True);
end;

--# services.adb services-flipx.adb services-flipb.adb
--  /flipx/ l+ 0
--  /flipb/ l- s-
