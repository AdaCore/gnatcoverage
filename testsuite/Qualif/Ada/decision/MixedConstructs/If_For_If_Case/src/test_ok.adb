with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_OK is
   S : aliased Sensor (Hist_Size => 5);
   
begin
   S.ALB := 1;
   S.AHB := 10;
   S.V := 5;
   
   Sample (S);
   Assert (Status_Of (S) = Ok);
end;

--# sensors-status.adb

-- /SO_t0/       l! ## dT-
-- /SO_undecide/ l- ## s-
-- /SO_decide/   l+ ## 0
-- /SO_loop0/    l+ ## 0
-- /SO_loop1/    l+ ## 0c
-- /SO_tfaultLB/ l! ## dT-
-- /SO_tfaultHB/ l! ## 0
-- /SO_fault/    l- ## s-
-- /SO_check/    l- ## s-
-- /SO_broken/   l- ## s-
-- /SO_PE/       l- ## s-
-- /SO_ret/      l+ ## 0
