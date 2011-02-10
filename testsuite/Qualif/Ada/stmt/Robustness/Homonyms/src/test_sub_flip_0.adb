with Support, Sub_Services; use Support, Sub_Services;

procedure Test_Sub_Flip_0 is
begin
   Dispatch (Do_Flipx => False, Do_Flipb => False);
end;

--# sub_services.adb sub_services-flipx.adb sub_services-flipb.adb
--  /flipx/ l- s-
--  /flipb/ l- s-
