with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_F_FF is
   S : Sensor;
   
begin
   -- Outer If False only. Inner If False only from F or else F.
   S.ALB := 1;
   S.AHB := 15;
   S.V := 5; -- in range
   
   Sample (S);
   Assert (Status_Of (S) = Ok);
end;

--# sensors-status.adb

-- /SO_t0/       l! d!
-- /SO_undecide/ l- s-
-- /SO_decide/   l+ 0
-- /SO_loop0/    l+ 0
-- /SO_loop1/    l+ 0c
-- /SO_tfaultLB/ l! dT-
-- /SO_tfaultHB/ l! 0
-- /SO_fault/    l- s-
-- /SO_check/    l- s-
-- /SO_broken/   l- s-
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
