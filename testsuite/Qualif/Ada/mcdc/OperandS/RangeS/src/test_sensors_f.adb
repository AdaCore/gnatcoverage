with Support, Sensors; use Support, Sensors;

procedure Test_Sensors_F is
begin
   Assert (Both_OK (OK, NOK) = False);
   Assert (Both_OK (NOK, OK) = False);
end;

--# sensors.adb
--  /evaluate/ l! dT-


