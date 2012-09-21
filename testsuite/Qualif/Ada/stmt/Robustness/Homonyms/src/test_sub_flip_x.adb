with Support, Sub_Services; use Support, Sub_Services;

procedure Test_Sub_Flip_X is
begin
   Dispatch (Do_Flipb => False, Do_Flipx => True);
end;

--# sub_services.adb sub_services-flipx.adb sub_services-flipb.adb
--  /flipx/ l+ ## 0
--  /flipb/ l- ## s-
