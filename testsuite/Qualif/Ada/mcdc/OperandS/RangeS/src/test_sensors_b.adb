with Support, Sensors; use Support, Sensors;

procedure Test_Sensors_B is
begin
   Assert (Both_OK (OK, OK) = True);
   Assert (Both_OK (OK, NOK) = False);
end;

--# sensors.adb
--  /evaluate/ l! m!:"A.V"

