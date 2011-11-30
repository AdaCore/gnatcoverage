with Support; use Support;
with Sensors.Status; use Sensors, Sensors.Status;

procedure Test_F_FU is
   S : Sensor;

begin
   -- Outer If False only. Inner If both True and False.

   S.ALB := 1;
   S.AHB := 15;
   S.V := 0; -- < low bound

   Sample (S);

   S.ALB := 1;
   S.AHB := 15;
   S.V := 5; -- in range

   Sample (S);
   Assert (Status_Of (S) = Check);
end;

--# sensors-status.adb

-- /SO_t0/       l! dT-
-- /SO_undecide/ l- s-
-- /SO_decide/   l+ 0
-- /SO_loop0/    l+ 0
-- /SO_loop1/    l+ 0c
-- /SO_tfaultLB/ l+ 0
-- /SO_tfaultHB/ l+ 0
-- /SO_fault/    l+ 0
-- /SO_check/    l+ 0
-- /SO_broken/   l- s-
-- /SO_PE/       l- s-
-- /SO_ret/      l+ 0
