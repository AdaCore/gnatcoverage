with Support, Services; use Support, Services;

procedure Test_Flip_0 is
begin
   Dispatch (Do_Flipx => False, Do_Flipb => False);
end;

--# services.adb services-flipx.adb services-flipb.adb
--  /flipx/ l- s-
--  /flipb/ l- s-
