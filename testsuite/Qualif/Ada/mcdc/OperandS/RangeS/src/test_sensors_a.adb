with Support, Sensors; use Support, Sensors;

procedure Test_Sensors_A is
begin
   Assert (Both_OK (OK, OK) = True);
   Assert (Both_OK (NOK, OK) = False);
end;

--# sensors.adb
--  /evaluate/ l! c!:"B.V"

