with Support, Sub_Services; use Support, Sub_Services;

procedure Test_Sub_Flip_B is
begin
   Dispatch (Do_Flipb => True, Do_Flipx => False);
end;

--# sub_services.adb sub_services-flipx.adb sub_services-flipb.adb
--  /flipx/ l- s-
--  /flipb/ l+ 0
