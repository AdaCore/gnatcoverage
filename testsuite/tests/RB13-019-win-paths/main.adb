with Utils;

procedure Main is
   procedure Driver_Main;
   pragma Import (C, Driver_Main, "driver_main");
begin
   Driver_Main;
end Main;
